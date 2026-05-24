from __future__ import annotations

import geopandas as gpd
import networkx as nx


def add_reach_basin_attributes(
    graph: nx.DiGraph,
    reaches: gpd.GeoDataFrame,
    basins: gpd.GeoDataFrame,
) -> None:
    reach_lookup = {int(row.reach_id): row for _, row in reaches.iterrows()}
    basin_lookup = {}
    if not basins.empty and "reach_id" in basins:
        basin_lookup = {int(row.reach_id): row for _, row in basins.dropna(subset=["reach_id"]).iterrows()}

    for node in graph.nodes:
        reach = reach_lookup.get(int(node))
        basin = basin_lookup.get(int(node))
        if reach is not None:
            graph.nodes[node]["reach_length"] = float(reach.geometry.length)
        if basin is not None:
            area = float(basin.geometry.area)
            graph.nodes[node]["basin_area"] = area
            graph.nodes[node]["drainage_density"] = graph.nodes[node].get("reach_length", 0.0) / area if area else 0.0
        graph.nodes[node].setdefault("mean_elevation", None)
        graph.nodes[node].setdefault("mean_slope", None)

