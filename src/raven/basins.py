from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path

import geopandas as gpd
from shapely.geometry import box

from raven.io import RasterData
from raven.streams import StreamCollection


@dataclass
class BasinCollection:
    basins: gpd.GeoDataFrame


def delineate_basins(
    fdir: RasterData,
    stream_collection: StreamCollection,
    mode: str = "confluences",
    pour_points_path: str | Path | None = None,
    snap_distance: float = 150.0,
) -> BasinCollection:
    """Create simple basin envelopes for reaches or supplied pour points."""
    if mode not in {"confluences", "pour_points"}:
        raise ValueError("basin mode must be 'confluences' or 'pour_points'")
    if mode == "pour_points" and pour_points_path:
        return _basins_from_pour_points(fdir, stream_collection, pour_points_path, snap_distance)
    return _basins_from_reaches(stream_collection)


def _basins_from_reaches(stream_collection: StreamCollection) -> BasinCollection:
    records = []
    for _, reach in stream_collection.reaches.iterrows():
        envelope = reach.geometry.buffer(reach.geometry.length * 0.25 + 1.0).envelope
        records.append(
            {
                "basin_id": f"b_{reach.reach_id}",
                "reach_id": int(reach.reach_id),
                "mode": "confluences",
                "geometry": envelope,
            }
        )
    return BasinCollection(gpd.GeoDataFrame(records, geometry="geometry", crs=stream_collection.reaches.crs))


def _basins_from_pour_points(
    fdir: RasterData,
    stream_collection: StreamCollection,
    pour_points_path: str | Path,
    snap_distance: float,
) -> BasinCollection:
    points = gpd.read_file(pour_points_path).to_crs(stream_collection.reaches.crs)
    stream_union = stream_collection.reaches.geometry.union_all()
    records = []

    for idx, point in points.iterrows():
        snapped = point.geometry
        if not stream_union.is_empty and point.geometry.distance(stream_union) <= snap_distance:
            snapped = stream_union.interpolate(stream_union.project(point.geometry))
        col, row = ~fdir.transform * (snapped.x, snapped.y)
        x0, y0 = fdir.transform * (int(col) - 2, int(row) - 2)
        x1, y1 = fdir.transform * (int(col) + 3, int(row) + 3)
        records.append(
            {
                "basin_id": f"gauge_{idx}",
                "reach_id": None,
                "mode": "pour_points",
                "geometry": box(min(x0, x1), min(y0, y1), max(x0, x1), max(y0, y1)),
            }
        )
    return BasinCollection(gpd.GeoDataFrame(records, geometry="geometry", crs=stream_collection.reaches.crs))

