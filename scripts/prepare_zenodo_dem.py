from __future__ import annotations

from pathlib import Path
from urllib.request import urlretrieve

import numpy as np
import rasterio
from rasterio.enums import Resampling
from rasterio.warp import calculate_default_transform, reproject
from rasterio.windows import Window

RECORD_URL = "https://zenodo.org/records/7797361"
DOWNLOAD_URL = "https://zenodo.org/records/7797361/files/DEM.tif?download=1"
RAW_PATH = Path("data/raw/zenodo_7797361_dem.tif")
PROCESSED_PATH = Path("data/processed/zenodo_7797361_dem_utm50n.tif")


def main() -> None:
    RAW_PATH.parent.mkdir(parents=True, exist_ok=True)
    PROCESSED_PATH.parent.mkdir(parents=True, exist_ok=True)
    if not RAW_PATH.exists():
        urlretrieve(DOWNLOAD_URL, RAW_PATH)
    prepare_projected_subset(RAW_PATH, PROCESSED_PATH)
    print(PROCESSED_PATH)


def prepare_projected_subset(raw_path: Path, output_path: Path) -> None:
    with rasterio.open(raw_path) as src:
        window = _best_relief_window(src, size=260, stride=220)
        data = src.read(1, window=window, masked=True)
        transform = src.window_transform(window)
        bounds = rasterio.windows.bounds(window, src.transform)

        dst_crs = "EPSG:32650"
        dst_transform, width, height = calculate_default_transform(
            src.crs,
            dst_crs,
            window.width,
            window.height,
            *bounds,
            resolution=90,
        )
        destination = np.full((height, width), src.nodata or -9999.0, dtype="float32")
        reproject(
            source=np.asarray(data.filled(src.nodata or -9999.0), dtype="float32"),
            destination=destination,
            src_transform=transform,
            src_crs=src.crs,
            src_nodata=src.nodata,
            dst_transform=dst_transform,
            dst_crs=dst_crs,
            dst_nodata=src.nodata or -9999.0,
            resampling=Resampling.bilinear,
        )

    profile = {
        "driver": "GTiff",
        "height": destination.shape[0],
        "width": destination.shape[1],
        "count": 1,
        "dtype": "float32",
        "crs": dst_crs,
        "transform": dst_transform,
        "nodata": src.nodata or -9999.0,
        "compress": "deflate",
    }
    with rasterio.open(output_path, "w", **profile) as dst:
        dst.write(destination, 1)
        dst.update_tags(
            zenodo_record=RECORD_URL,
            source_file="DEM.tif",
            derived_product="projected subset for RAVEN example workflow",
        )


def _best_relief_window(src: rasterio.DatasetReader, size: int, stride: int) -> Window:
    best_window = Window(0, 0, min(size, src.width), min(size, src.height))
    best_score = -1.0
    for row in range(0, max(1, src.height - size), stride):
        for col in range(0, max(1, src.width - size), stride):
            window = Window(col, row, size, size)
            data = src.read(1, window=window, masked=True)
            if data.count() == 0:
                continue
            score = float(data.max() - data.min())
            if score > best_score:
                best_score = score
                best_window = window
    return best_window


if __name__ == "__main__":
    main()
