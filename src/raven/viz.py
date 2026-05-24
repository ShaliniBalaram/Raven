from __future__ import annotations

from pathlib import Path

import geopandas as gpd
import numpy as np
from PIL import Image, ImageDraw

from raven.io import RasterData


def quicklook(
    dem: RasterData,
    conditioned: RasterData,
    fdir: RasterData,
    facc: RasterData,
    streams: gpd.GeoDataFrame,
    basins: gpd.GeoDataFrame,
    output_path: str | Path,
) -> None:
    panels = [
        _panel(dem.array, "DEM"),
        _panel(conditioned.array, "Conditioned DEM"),
        _panel(np.log1p(facc.array), "Flow Accumulation"),
        _panel(fdir.array, "D8 Direction"),
        _overlay_panel(dem, streams, basins, "Basins + Streams"),
        _summary_panel(dem, streams, basins, facc),
    ]
    fig = Image.new("RGB", (960, 640), "white")
    for idx, panel in enumerate(panels):
        x = (idx % 3) * 320
        y = (idx // 3) * 320
        fig.paste(panel, (x, y))
    Path(output_path).parent.mkdir(parents=True, exist_ok=True)
    fig.save(output_path)


def _panel(array: np.ndarray, title: str) -> Image.Image:
    image = _array_to_image(array).resize((300, 270))
    panel = Image.new("RGB", (320, 320), "white")
    panel.paste(image, (10, 40))
    ImageDraw.Draw(panel).text((10, 12), title, fill=(30, 30, 30))
    return panel


def _overlay_panel(dem: RasterData, streams: gpd.GeoDataFrame, basins: gpd.GeoDataFrame, title: str) -> Image.Image:
    panel = _panel(dem.array, title)
    draw = ImageDraw.Draw(panel)

    def to_pixel(x: float, y: float) -> tuple[int, int]:
        col, row = ~dem.transform * (x, y)
        return int(10 + col * 300 / dem.array.shape[1]), int(40 + row * 270 / dem.array.shape[0])

    for geom in basins.geometry:
        boundary = geom.boundary
        lines = list(boundary.geoms) if hasattr(boundary, "geoms") else [boundary]
        for line in lines:
            draw.line([to_pixel(x, y) for x, y in line.coords], fill=(76, 120, 168), width=1)
    for geom in streams.geometry:
        draw.line([to_pixel(x, y) for x, y in geom.coords], fill=(245, 133, 24), width=2)
    return panel


def _summary_panel(dem: RasterData, streams: gpd.GeoDataFrame, basins: gpd.GeoDataFrame, facc: RasterData) -> Image.Image:
    panel = Image.new("RGB", (320, 320), "white")
    draw = ImageDraw.Draw(panel)
    draw.text((10, 12), "Summary", fill=(30, 30, 30))
    lines = [
        f"reaches: {len(streams)}",
        f"basins: {len(basins)}",
        f"max accumulation: {float(np.nanmax(facc.array)):.0f}",
        f"cell area: {dem.cell_area:.2f}",
    ]
    for idx, line in enumerate(lines):
        draw.text((20, 70 + idx * 30), line, fill=(45, 45, 45))
    return panel


def _array_to_image(array: np.ndarray) -> Image.Image:
    values = np.asarray(array, dtype="float64")
    values = np.where(np.isfinite(values), values, np.nan)
    min_value = np.nanmin(values)
    max_value = np.nanmax(values)
    if max_value == min_value:
        scaled = np.zeros(values.shape, dtype="uint8")
    else:
        scaled = ((values - min_value) / (max_value - min_value) * 255).astype("uint8")
    return Image.fromarray(scaled, mode="L").convert("RGB")
