from __future__ import annotations

from pathlib import Path
from urllib.request import urlretrieve
from zipfile import ZipFile

import rasterio
from rasterio.windows import Window

RECORD_URL = "https://zenodo.org/records/3940482"
DOWNLOAD_URL = "https://zenodo.org/records/3940482/files/Gore_Range_GeoTIFF.zip?download=1"
ARCHIVE_PATH = Path("data/raw/gore_range_geotiff.zip")
SOURCE_MEMBER = "Gore_Range_GeoTIFF/Gore_Range_Albers_250m/Gore_Range_Albers_250m.tif"
TEMP_SOURCE = Path("data/raw/gore_range_albers_250m.tif")
PROCESSED_PATH = Path("data/processed/gore_range_albers_250m_subset.tif")


def main() -> None:
    ARCHIVE_PATH.parent.mkdir(parents=True, exist_ok=True)
    PROCESSED_PATH.parent.mkdir(parents=True, exist_ok=True)
    if not ARCHIVE_PATH.exists():
        urlretrieve(DOWNLOAD_URL, ARCHIVE_PATH)
    extract_source_tif()
    prepare_subset(TEMP_SOURCE, PROCESSED_PATH)
    print(PROCESSED_PATH)


def extract_source_tif() -> None:
    if TEMP_SOURCE.exists():
        return
    with ZipFile(ARCHIVE_PATH) as archive:
        with archive.open(SOURCE_MEMBER) as source, TEMP_SOURCE.open("wb") as target:
            target.write(source.read())


def prepare_subset(source_path: Path, output_path: Path) -> None:
    with rasterio.open(source_path) as src:
        window = best_relief_window(src, size=260, stride=160)
        data = src.read(1, window=window)
        profile = src.profile.copy()
        profile.update(
            height=window.height,
            width=window.width,
            transform=src.window_transform(window),
            compress="deflate",
        )

    with rasterio.open(output_path, "w", **profile) as dst:
        dst.write(data, 1)
        dst.update_tags(
            zenodo_record=RECORD_URL,
            source_file=SOURCE_MEMBER,
            derived_product="projected Gore Range subset for RAVEN example workflow",
        )


def best_relief_window(src: rasterio.DatasetReader, size: int, stride: int) -> Window:
    best_window = Window(0, 0, min(size, src.width), min(size, src.height))
    best_score = -1.0
    for row in range(0, max(1, src.height - size), stride):
        for col in range(0, max(1, src.width - size), stride):
            window = Window(col, row, size, size)
            data = src.read(1, window=window, masked=True)
            if data.count() < size * size * 0.95:
                continue
            score = float(data.max() - data.min())
            if score > best_score:
                best_score = score
                best_window = window
    return best_window


if __name__ == "__main__":
    main()
