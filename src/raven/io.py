from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Any

import geopandas as gpd
import numpy as np
import rasterio
from rasterio.mask import mask
from rasterio.warp import Resampling, calculate_default_transform, reproject


@dataclass
class RasterData:
    array: np.ndarray
    transform: Any
    crs: Any
    nodata: float | int | None = None
    name: str = "raster"

    @property
    def profile(self) -> dict[str, Any]:
        return {
            "driver": "GTiff",
            "height": self.array.shape[0],
            "width": self.array.shape[1],
            "count": 1,
            "dtype": self.array.dtype,
            "crs": self.crs,
            "transform": self.transform,
            "nodata": self.nodata,
        }

    @property
    def cell_area(self) -> float:
        return abs(self.transform.a * self.transform.e)


@dataclass
class OutputPaths:
    streams: Path
    basins: Path
    graphs: Path
    figures: Path

    @classmethod
    def from_config(cls, config: dict[str, Any]) -> "OutputPaths":
        outputs = config.get("outputs", {})
        root = Path(outputs.get("root", "outputs"))
        return cls(
            streams=Path(outputs.get("streams", root / "streams")),
            basins=Path(outputs.get("basins", root / "basins")),
            graphs=Path(outputs.get("graphs", root / "graphs")),
            figures=Path(outputs.get("figures", root / "figures")),
        )

    def ensure_dirs(self) -> None:
        for path in (self.streams, self.basins, self.graphs, self.figures):
            path.mkdir(parents=True, exist_ok=True)


def load_dem_clipped(
    dem_path: str | Path,
    aoi_path: str | Path | None = None,
    target_crs: str | None = None,
) -> RasterData:
    """Load a DEM, optionally clip by AOI, optionally reproject, and validate CRS."""
    path = Path(dem_path)
    with rasterio.open(path) as src:
        if aoi_path:
            aoi = gpd.read_file(aoi_path).to_crs(src.crs)
            array, transform = mask(src, aoi.geometry, crop=True, filled=True)
            crs = src.crs
            nodata = src.nodata
            profile = src.profile.copy()
            profile.update(height=array.shape[1], width=array.shape[2], transform=transform)
        else:
            array = src.read(1, masked=False)[np.newaxis, ...]
            transform = src.transform
            crs = src.crs
            nodata = src.nodata
            profile = src.profile.copy()

    dem = RasterData(array=array[0], transform=transform, crs=crs, nodata=nodata, name=path.stem)
    if target_crs and str(dem.crs) != target_crs:
        dem = reproject_raster(dem, target_crs, profile)
    ensure_projected_crs(dem)
    return dem


def reproject_raster(raster: RasterData, target_crs: str, profile: dict[str, Any]) -> RasterData:
    transform, width, height = calculate_default_transform(
        raster.crs,
        target_crs,
        profile["width"],
        profile["height"],
        *rasterio.transform.array_bounds(profile["height"], profile["width"], raster.transform),
    )
    destination = np.empty((height, width), dtype=raster.array.dtype)
    reproject(
        source=raster.array,
        destination=destination,
        src_transform=raster.transform,
        src_crs=raster.crs,
        dst_transform=transform,
        dst_crs=target_crs,
        resampling=Resampling.bilinear,
        src_nodata=raster.nodata,
        dst_nodata=raster.nodata,
    )
    return RasterData(destination, transform, target_crs, raster.nodata, raster.name)


def ensure_projected_crs(raster: RasterData) -> None:
    if raster.crs is None:
        raise ValueError("DEM has no CRS. Assign a projected CRS before processing.")
    crs = rasterio.crs.CRS.from_user_input(raster.crs)
    if crs.is_geographic:
        raise ValueError("DEM CRS is geographic. Reproject to a projected CRS before processing.")

