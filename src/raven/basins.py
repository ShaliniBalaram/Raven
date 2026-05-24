from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path

import geopandas as gpd
import numpy as np
from rasterio.features import shapes
from shapely.geometry import box
from shapely.geometry import shape

from raven.flow import D8_OFFSETS
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
    """Delineate raster drainage areas for traced reaches."""
    if mode not in {"confluences", "pour_points"}:
        raise ValueError("basin mode must be 'confluences' or 'pour_points'")
    if mode == "pour_points" and pour_points_path:
        return _basins_from_pour_points(fdir, stream_collection, pour_points_path, snap_distance)
    return _basins_from_flow_paths(fdir, stream_collection)


def _basins_from_flow_paths(fdir: RasterData, stream_collection: StreamCollection) -> BasinCollection:
    if not stream_collection.cell_to_reach:
        return _empty_basins(stream_collection)

    labels = np.zeros(fdir.array.shape, dtype="int32")
    downstream = _downstream_cells(fdir.array)
    for row in range(fdir.array.shape[0]):
        for col in range(fdir.array.shape[1]):
            reach_id = _first_downstream_reach((row, col), downstream, stream_collection.cell_to_reach)
            if reach_id is not None:
                labels[row, col] = reach_id

    records = []
    for geom, value in shapes(labels, mask=labels > 0, transform=fdir.transform):
        reach_id = int(value)
        records.append({"basin_id": f"b_{reach_id}", "reach_id": reach_id, "mode": "flow_path", "geometry": shape(geom)})

    if not records:
        return _empty_basins(stream_collection)

    basins = gpd.GeoDataFrame(records, geometry="geometry", crs=fdir.crs)
    dissolved = basins.dissolve(by="reach_id", as_index=False, aggfunc="first")
    dissolved["basin_id"] = dissolved["reach_id"].map(lambda value: f"b_{int(value)}")
    dissolved["mode"] = "flow_path"
    return BasinCollection(dissolved[["basin_id", "reach_id", "mode", "geometry"]])


def _downstream_cells(direction: np.ndarray) -> dict[tuple[int, int], tuple[int, int]]:
    downstream = {}
    rows, cols = direction.shape
    for row in range(rows):
        for col in range(cols):
            code = int(direction[row, col])
            if code not in D8_OFFSETS:
                continue
            drow, dcol = D8_OFFSETS[code]
            target = (row + drow, col + dcol)
            if 0 <= target[0] < rows and 0 <= target[1] < cols:
                downstream[(row, col)] = target
    return downstream


def _first_downstream_reach(
    cell: tuple[int, int],
    downstream: dict[tuple[int, int], tuple[int, int]],
    cell_to_reach: dict[tuple[int, int], int],
) -> int | None:
    current = cell
    visited = set()
    while current not in visited:
        if current in cell_to_reach:
            return cell_to_reach[current]
        visited.add(current)
        if current not in downstream:
            return None
        current = downstream[current]
    return None


def _empty_basins(stream_collection: StreamCollection) -> BasinCollection:
    return BasinCollection(
        gpd.GeoDataFrame(columns=["basin_id", "reach_id", "mode", "geometry"], geometry="geometry", crs=stream_collection.reaches.crs)
    )


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
