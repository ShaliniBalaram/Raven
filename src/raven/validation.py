from __future__ import annotations

import json
from pathlib import Path

import geopandas as gpd
import numpy as np
import rasterio
from PIL import Image, ImageDraw
from shapely.geometry import box


def validate_streams(
    raven_streams_path: str | Path,
    reference_streams_path: str | Path,
    dem_path: str | Path,
    output_dir: str | Path = "outputs/validation",
    tolerance: float = 250.0,
) -> dict:
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    raven = gpd.read_file(raven_streams_path)
    reference = gpd.read_file(reference_streams_path)
    with rasterio.open(dem_path) as dem:
        dem_crs = dem.crs
        bounds = box(*dem.bounds)
        dem_array = dem.read(1, masked=True)
        transform = dem.transform

    raven = gpd.clip(raven.to_crs(dem_crs), gpd.GeoSeries([bounds], crs=dem_crs))
    reference = gpd.clip(reference.to_crs(dem_crs), gpd.GeoSeries([bounds], crs=dem_crs))

    if raven.empty or reference.empty:
        raise ValueError("RAVEN streams and reference streams must both intersect the DEM extent.")

    metrics = _stream_metrics(raven, reference, tolerance)
    report_path = output_dir / "stream_validation.json"
    figure_path = output_dir / "stream_validation.png"
    report_path.write_text(json.dumps(metrics, indent=2), encoding="utf-8")
    _validation_figure(dem_array, transform, raven, reference, figure_path)

    metrics["report"] = str(report_path)
    metrics["figure"] = str(figure_path)
    return metrics


def _stream_metrics(raven: gpd.GeoDataFrame, reference: gpd.GeoDataFrame, tolerance: float) -> dict:
    raven_union = raven.geometry.union_all()
    reference_union = reference.geometry.union_all()
    raven_lengths = raven.length
    reference_lengths = reference.length
    distances = raven.geometry.apply(lambda geom: geom.distance(reference_union))
    matched = raven.geometry.apply(lambda geom: geom.intersects(reference_union.buffer(tolerance)))

    return {
        "raven_reach_count": int(len(raven)),
        "reference_feature_count": int(len(reference)),
        "raven_total_length": float(raven_lengths.sum()),
        "reference_total_length": float(reference_lengths.sum()),
        "length_ratio": float(raven_lengths.sum() / reference_lengths.sum()) if reference_lengths.sum() else None,
        "mean_nearest_reference_distance": float(distances.mean()),
        "median_nearest_reference_distance": float(distances.median()),
        "max_nearest_reference_distance": float(distances.max()),
        "matched_length_within_tolerance": float(raven.loc[matched, "geometry"].length.sum()),
        "matched_length_fraction": float(raven.loc[matched, "geometry"].length.sum() / raven_lengths.sum()) if raven_lengths.sum() else None,
        "reference_coverage_fraction": float(reference_union.buffer(tolerance).intersection(raven_union).length / raven_lengths.sum())
        if raven_lengths.sum()
        else None,
        "tolerance": float(tolerance),
    }


def _validation_figure(dem_array, transform, raven: gpd.GeoDataFrame, reference: gpd.GeoDataFrame, output_path: Path) -> None:
    base = _array_to_image(np.asarray(dem_array.filled(np.nan), dtype="float64")).resize((900, 900))
    draw = ImageDraw.Draw(base)

    def to_pixel(x: float, y: float) -> tuple[int, int]:
        col, row = ~transform * (x, y)
        return int(col * 900 / dem_array.shape[1]), int(row * 900 / dem_array.shape[0])

    for geom in reference.geometry:
        _draw_geometry(draw, geom, to_pixel, fill=(35, 120, 210), width=3)
    for geom in raven.geometry:
        _draw_geometry(draw, geom, to_pixel, fill=(245, 133, 24), width=2)

    draw.rectangle((12, 12, 315, 82), fill=(255, 255, 255), outline=(170, 170, 170))
    draw.line((28, 34, 92, 34), fill=(35, 120, 210), width=4)
    draw.text((105, 25), "Reference streams", fill=(30, 30, 30))
    draw.line((28, 62, 92, 62), fill=(245, 133, 24), width=4)
    draw.text((105, 53), "RAVEN streams", fill=(30, 30, 30))
    base.save(output_path)


def _draw_geometry(draw: ImageDraw.ImageDraw, geom, to_pixel, fill, width: int) -> None:
    if geom.is_empty:
        return
    if geom.geom_type == "LineString":
        draw.line([to_pixel(x, y) for x, y in geom.coords], fill=fill, width=width)
    elif geom.geom_type == "MultiLineString":
        for line in geom.geoms:
            _draw_geometry(draw, line, to_pixel, fill, width)


def _array_to_image(array: np.ndarray) -> Image.Image:
    values = np.where(np.isfinite(array), array, np.nan)
    min_value = np.nanmin(values)
    max_value = np.nanmax(values)
    if max_value == min_value:
        scaled = np.zeros(values.shape, dtype="uint8")
    else:
        scaled = ((values - min_value) / (max_value - min_value) * 255).astype("uint8")
    return Image.fromarray(scaled, mode="L").convert("RGB")
