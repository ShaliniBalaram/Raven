from __future__ import annotations

import numpy as np

from raven.io import RasterData


def fill_pits(dem: RasterData, method: str = "pysheds") -> RasterData:
    """Fill single-cell pits with a compact priority-style iterative pass."""
    if method not in {"pysheds", "whitebox", "simple"}:
        raise ValueError(f"Unsupported conditioning method: {method}")

    filled = np.array(dem.array, dtype="float64", copy=True)
    valid = np.ones(filled.shape, dtype=bool)
    if dem.nodata is not None:
        valid &= filled != dem.nodata
    valid &= np.isfinite(filled)

    for _ in range(32):
        changed = False
        previous = filled.copy()
        for row in range(1, filled.shape[0] - 1):
            for col in range(1, filled.shape[1] - 1):
                if not valid[row, col]:
                    continue
                window = previous[row - 1 : row + 2, col - 1 : col + 2]
                mask = valid[row - 1 : row + 2, col - 1 : col + 2].copy()
                mask[1, 1] = False
                neighbors = window[mask]
                if neighbors.size and previous[row, col] < neighbors.min():
                    filled[row, col] = neighbors.min()
                    changed = True
        if not changed:
            break

    return RasterData(filled, dem.transform, dem.crs, dem.nodata, f"{dem.name}_conditioned")

