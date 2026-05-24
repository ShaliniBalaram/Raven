from __future__ import annotations

from pathlib import Path

import numpy as np
import rasterio
from rasterio.transform import from_origin


def main() -> None:
    output_path = Path("data/raw/sample_dem.tif")
    output_path.parent.mkdir(parents=True, exist_ok=True)

    rows, cols = 60, 60
    y, x = np.mgrid[0:rows, 0:cols]
    base = 920 - (x * 4.5) - (y * 3.5)
    valley = -85 * np.exp(-((x - y * 0.72 - 8) ** 2) / 55)
    tributary = -38 * np.exp(-((x + y - 70) ** 2) / 80)
    ridge = 25 * np.sin(x / 7) + 18 * np.cos(y / 9)
    dem = (base + valley + tributary + ridge).astype("float32")

    profile = {
        "driver": "GTiff",
        "height": rows,
        "width": cols,
        "count": 1,
        "dtype": "float32",
        "crs": "EPSG:32643",
        "transform": from_origin(500000, 1900000, 30, 30),
        "nodata": -9999.0,
    }

    with rasterio.open(output_path, "w", **profile) as dst:
        dst.write(dem, 1)

    print(output_path)


if __name__ == "__main__":
    main()
