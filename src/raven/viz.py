from __future__ import annotations

from pathlib import Path

import geopandas as gpd
import matplotlib.pyplot as plt

from raven.io import RasterData


def quicklook(
    dem: RasterData,
    conditioned: RasterData,
    fdir: RasterData,
    facc: RasterData,
    streams: gpd.GeoDataFrame,
    basins: gpd.GeoDataFrame,
    output_path: str | Path,
) -> None:
    fig, axes = plt.subplots(2, 3, figsize=(13, 8), constrained_layout=True)
    panels = axes.ravel()

    panels[0].imshow(dem.array, cmap="terrain")
    panels[0].set_title("DEM")
    panels[1].imshow(conditioned.array, cmap="terrain")
    panels[1].set_title("Conditioned DEM")
    panels[2].imshow(facc.array, cmap="magma")
    panels[2].set_title("Flow Accumulation")
    panels[3].imshow(fdir.array, cmap="tab20")
    panels[3].set_title("D8 Direction")

    basins.boundary.plot(ax=panels[4], color="#4C78A8", linewidth=0.8)
    streams.plot(ax=panels[4], color="#F58518", linewidth=1.0)
    panels[4].set_title("Basins + Streams")

    panels[5].axis("off")
    panels[5].text(
        0.02,
        0.9,
        "\n".join(
            [
                f"reaches: {len(streams)}",
                f"basins: {len(basins)}",
                f"max accumulation: {float(facc.array.max()):.0f}",
                f"cell area: {dem.cell_area:.2f}",
            ]
        ),
        va="top",
        fontsize=12,
    )
    panels[5].set_title("Summary")

    for ax in panels[:5]:
        ax.set_xticks([])
        ax.set_yticks([])

    Path(output_path).parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(output_path, dpi=180)
    plt.close(fig)

