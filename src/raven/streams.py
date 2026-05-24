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


def extract_streams(facc: RasterData, threshold: int) -> StreamCollection:
    """Vectorize thresholded accumulation cells into simple D8 reach segments."""
    stream_mask = np.asarray(facc.array) >= threshold
    records = []
    reach_id = 1

    for row in range(stream_mask.shape[0]):
        for col in range(stream_mask.shape[1]):
            if not stream_mask[row, col]:
                continue
            end = _best_downstream_stream_cell(facc.array, stream_mask, row, col)
            if end is None:
                continue
            start_xy = _cell_center(facc.transform, row, col)
            end_xy = _cell_center(facc.transform, end[0], end[1])
            records.append(
                {
                    "reach_id": reach_id,
                    "from_node": f"n_{row}_{col}",
                    "to_node": f"n_{end[0]}_{end[1]}",
                    "accumulation": float(facc.array[row, col]),
                    "geometry": LineString([start_xy, end_xy]),
                }
            )
            reach_id += 1

    gdf = gpd.GeoDataFrame(records, geometry="geometry", crs=facc.crs)
    return StreamCollection(gdf)


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

