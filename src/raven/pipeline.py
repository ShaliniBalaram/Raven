from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Any

import yaml

from raven import attributes, basins, condition, flow, graph, io, ordering, streams, viz


@dataclass
class Pipeline:
    """Coordinate the RAVEN processing stages for one area of interest."""

    config: dict[str, Any]

    @classmethod
    def from_yaml(cls, path: str | Path) -> "Pipeline":
        with Path(path).open("r", encoding="utf-8") as handle:
            config = yaml.safe_load(handle) or {}
        config.setdefault("name", Path(path).stem)
        config.setdefault("outputs", {})
        return cls(config)

    @property
    def name(self) -> str:
        return str(self.config["name"])

    def load_dem(self) -> io.RasterData:
        return io.load_dem_clipped(
            self.config["dem_path"],
            aoi_path=self.config.get("aoi_path"),
            target_crs=self.config.get("target_crs"),
        )

    def condition_dem(self, dem: io.RasterData) -> io.RasterData:
        return condition.fill_pits(dem, method=self.config.get("conditioning", {}).get("method", "pysheds"))

    def flow_direction(self, dem: io.RasterData) -> io.RasterData:
        return flow.flow_direction(dem, method=self.config.get("flow", {}).get("method", "pyflwdir"))

    def flow_accumulation(self, fdir: io.RasterData) -> io.RasterData:
        return flow.flow_accumulation(fdir)

    def extract_streams(self, facc: io.RasterData) -> streams.StreamCollection:
        threshold = int(self.config.get("streams", {}).get("accumulation_threshold", 1000))
        return streams.extract_streams(facc, threshold=threshold)

    def delineate_basins(
        self,
        fdir: io.RasterData,
        stream_collection: streams.StreamCollection,
    ) -> basins.BasinCollection:
        basin_config = self.config.get("basins", {})
        return basins.delineate_basins(
            fdir,
            stream_collection,
            mode=basin_config.get("mode", "confluences"),
            pour_points_path=basin_config.get("pour_points_path"),
            snap_distance=float(basin_config.get("snap_distance", 150.0)),
        )

    def build_graph(
        self,
        stream_collection: streams.StreamCollection,
        basin_collection: basins.BasinCollection,
    ):
        reach_graph = graph.build_reach_graph(stream_collection.reaches, basin_collection.basins)
        ordering.add_all_orderings(reach_graph)
        attributes.add_reach_basin_attributes(reach_graph, stream_collection.reaches, basin_collection.basins)
        return reach_graph

    def write_outputs(
        self,
        dem: io.RasterData,
        conditioned: io.RasterData,
        fdir: io.RasterData,
        facc: io.RasterData,
        stream_collection: streams.StreamCollection,
        basin_collection: basins.BasinCollection,
        reach_graph,
    ) -> dict[str, Path]:
        outputs = io.OutputPaths.from_config(self.config)
        outputs.ensure_dirs()

        stream_path = outputs.streams / f"{self.name}_streams.gpkg"
        basin_path = outputs.basins / f"{self.name}_basins.gpkg"
        graphml_path = outputs.graphs / f"{self.name}_graph.graphml"
        json_path = outputs.graphs / f"{self.name}_graph.json"
        figure_path = outputs.figures / f"{self.name}_quicklook.png"

        stream_collection.reaches.to_file(stream_path, driver="GPKG")
        basin_collection.basins.to_file(basin_path, driver="GPKG")
        graph.write_graphml(reach_graph, graphml_path)
        graph.write_json(reach_graph, json_path)
        viz.quicklook(dem, conditioned, fdir, facc, stream_collection.reaches, basin_collection.basins, figure_path)

        return {
            "streams": stream_path,
            "basins": basin_path,
            "graphml": graphml_path,
            "json": json_path,
            "quicklook": figure_path,
        }

    def run(self) -> dict[str, Path]:
        dem = self.load_dem()
        conditioned = self.condition_dem(dem)
        fdir = self.flow_direction(conditioned)
        facc = self.flow_accumulation(fdir)
        stream_collection = self.extract_streams(facc)
        basin_collection = self.delineate_basins(fdir, stream_collection)
        reach_graph = self.build_graph(stream_collection, basin_collection)
        return self.write_outputs(dem, conditioned, fdir, facc, stream_collection, basin_collection, reach_graph)

