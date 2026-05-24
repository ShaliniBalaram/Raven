from __future__ import annotations

from dataclasses import dataclass

import geopandas as gpd
import numpy as np
from shapely.geometry import LineString

from raven.flow import D8_OFFSETS
from raven.io import RasterData


@dataclass
class StreamCollection:
    reaches: gpd.GeoDataFrame
    cell_to_reach: dict[tuple[int, int], int]


def extract_streams(facc: RasterData, threshold: int, fdir: RasterData | None = None) -> StreamCollection:
    """Trace stream reaches through thresholded accumulation cells."""
    stream_mask = np.asarray(facc.array) >= threshold
    if fdir is None:
        return _extract_by_accumulation_gradient(facc, stream_mask)

    downstream = _downstream_cells(fdir.array)
    upstream_count = _stream_upstream_counts(stream_mask, downstream)
    starts = [
        cell
        for cell in downstream
        if stream_mask[cell] and upstream_count[cell] != 1
    ]

    records = []
    cell_to_reach: dict[tuple[int, int], int] = {}
    seen_paths: set[tuple[tuple[int, int], tuple[int, int]]] = set()
    reach_id = 1

    for start in starts:
        traced = _trace_reach(start, stream_mask, downstream, upstream_count)
        if len(traced) < 2:
            continue
        path_key = (traced[0], traced[-1])
        if path_key in seen_paths:
            continue
        seen_paths.add(path_key)
        for cell in traced:
            cell_to_reach.setdefault(cell, reach_id)
        records.append(_reach_record(facc, reach_id, traced))
        reach_id += 1

    if not records:
        gdf = _empty_reaches(facc.crs)
    else:
        gdf = gpd.GeoDataFrame(records, geometry="geometry", crs=facc.crs)
    return StreamCollection(gdf, cell_to_reach)


def _extract_by_accumulation_gradient(facc: RasterData, stream_mask: np.ndarray) -> StreamCollection:
    records = []
    cell_to_reach: dict[tuple[int, int], int] = {}
    reach_id = 1
    for row in range(stream_mask.shape[0]):
        for col in range(stream_mask.shape[1]):
            if not stream_mask[row, col]:
                continue
            end = _best_downstream_stream_cell(facc.array, stream_mask, row, col)
            if end is None:
                continue
            traced = [(row, col), end]
            cell_to_reach.setdefault((row, col), reach_id)
            cell_to_reach.setdefault(end, reach_id)
            records.append(_reach_record(facc, reach_id, traced))
            reach_id += 1
    if not records:
        return StreamCollection(_empty_reaches(facc.crs), cell_to_reach)
    return StreamCollection(gpd.GeoDataFrame(records, geometry="geometry", crs=facc.crs), cell_to_reach)


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


def _stream_upstream_counts(
    stream_mask: np.ndarray,
    downstream: dict[tuple[int, int], tuple[int, int]],
) -> dict[tuple[int, int], int]:
    counts = {cell: 0 for cell in downstream if stream_mask[cell]}
    for cell, target in downstream.items():
        if stream_mask[cell] and stream_mask[target]:
            counts[target] = counts.get(target, 0) + 1
    return counts


def _trace_reach(
    start: tuple[int, int],
    stream_mask: np.ndarray,
    downstream: dict[tuple[int, int], tuple[int, int]],
    upstream_count: dict[tuple[int, int], int],
) -> list[tuple[int, int]]:
    cells = [start]
    current = start
    visited = {start}
    while current in downstream:
        target = downstream[current]
        if target in visited or not stream_mask[target]:
            break
        cells.append(target)
        visited.add(target)
        if len(cells) > 1 and upstream_count.get(target, 0) != 1:
            break
        current = target
    return cells


def _reach_record(facc: RasterData, reach_id: int, cells: list[tuple[int, int]]) -> dict:
    start = cells[0]
    end = cells[-1]
    coords = [_cell_center(facc.transform, row, col) for row, col in cells]
    return {
        "reach_id": reach_id,
        "from_node": f"n_{start[0]}_{start[1]}",
        "to_node": f"n_{end[0]}_{end[1]}",
        "from_row": start[0],
        "from_col": start[1],
        "to_row": end[0],
        "to_col": end[1],
        "cell_count": len(cells),
        "accumulation": float(facc.array[end]),
        "geometry": LineString(coords),
    }


def _empty_reaches(crs) -> gpd.GeoDataFrame:
    return gpd.GeoDataFrame(
        columns=[
            "reach_id",
            "from_node",
            "to_node",
            "from_row",
            "from_col",
            "to_row",
            "to_col",
            "cell_count",
            "accumulation",
            "geometry",
        ],
        geometry="geometry",
        crs=crs,
    )


def _best_downstream_stream_cell(
    accumulation: np.ndarray,
    stream_mask: np.ndarray,
    row: int,
    col: int,
) -> tuple[int, int] | None:
    best_cell = None
    best_value = accumulation[row, col]
    for drow, dcol in D8_OFFSETS.values():
        rr = row + drow
        cc = col + dcol
        if 0 <= rr < stream_mask.shape[0] and 0 <= cc < stream_mask.shape[1]:
            if stream_mask[rr, cc] and accumulation[rr, cc] > best_value:
                best_value = accumulation[rr, cc]
                best_cell = (rr, cc)
    return best_cell


def _cell_center(transform, row: int, col: int) -> tuple[float, float]:
    x, y = transform * (col + 0.5, row + 0.5)
    return float(x), float(y)
