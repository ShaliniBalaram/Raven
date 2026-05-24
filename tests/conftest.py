from __future__ import annotations

import numpy as np
import pytest
from affine import Affine

from raven.io import RasterData


@pytest.fixture()
def tiny_dem() -> RasterData:
    array = np.array(
        [
            [10, 9, 8, 7, 6],
            [11, 8, 7, 6, 5],
            [12, 9, 4, 3, 2],
            [13, 10, 5, 2, 1],
            [14, 11, 6, 3, 0],
        ],
        dtype="float64",
    )
    return RasterData(array=array, transform=Affine.translation(0, 5) * Affine.scale(10, -10), crs="EPSG:32643")

