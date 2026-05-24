from __future__ import annotations

import numpy as np

from raven.io import RasterData

D8_OFFSETS = {
    1: (0, 1),
    2: (1, 1),
    4: (1, 0),
    8: (1, -1),
    16: (0, -1),
    32: (-1, -1),
    64: (-1, 0),
    128: (-1, 1),
}


def flow_direction(dem: RasterData, method: str = "pyflwdir") -> RasterData:
    """Calculate D8 direction codes toward the steepest downslope neighbor."""
    if method not in {"pyflwdir", "pysheds", "simple"}:
        raise ValueError(f"Unsupported flow method: {method}")

    array = dem.array.astype("float64")
    fdir = np.zeros(array.shape, dtype="uint8")
    dy = abs(dem.transform.e)
    dx = abs(dem.transform.a)

    for row in range(1, array.shape[0] - 1):
        for col in range(1, array.shape[1] - 1):
            center = array[row, col]
            if not np.isfinite(center):
                continue
            best_code = 0
            best_drop = 0.0
            for code, (drow, dcol) in D8_OFFSETS.items():
                neighbor = array[row + drow, col + dcol]
                distance = (dx**2 + dy**2) ** 0.5 if drow and dcol else (dy if drow else dx)
                drop = (center - neighbor) / distance
                if np.isfinite(neighbor) and drop > best_drop:
                    best_drop = drop
                    best_code = code
            fdir[row, col] = best_code

    return RasterData(fdir, dem.transform, dem.crs, 0, f"{dem.name}_fdir")


def flow_accumulation(fdir: RasterData) -> RasterData:
    """Count upstream cells for each D8 cell using topological relaxation."""
    direction = fdir.array.astype("uint8")
    rows, cols = direction.shape
    accumulation = np.ones(direction.shape, dtype="float64")
    indegree = np.zeros(direction.shape, dtype="int32")
    downstream: dict[tuple[int, int], tuple[int, int]] = {}

    for row in range(rows):
        for col in range(cols):
            code = int(direction[row, col])
            if code not in D8_OFFSETS:
                continue
            drow, dcol = D8_OFFSETS[code]
            target = (row + drow, col + dcol)
            if 0 <= target[0] < rows and 0 <= target[1] < cols:
                downstream[(row, col)] = target
                indegree[target] += 1

    queue = [(row, col) for row in range(rows) for col in range(cols) if indegree[row, col] == 0]
    head = 0
    while head < len(queue):
        cell = queue[head]
        head += 1
        target = downstream.get(cell)
        if target is None:
            continue
        accumulation[target] += accumulation[cell]
        indegree[target] -= 1
        if indegree[target] == 0:
            queue.append(target)

    return RasterData(accumulation, fdir.transform, fdir.crs, None, f"{fdir.name}_facc")

