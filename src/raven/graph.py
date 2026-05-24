from __future__ import annotations

import json
from pathlib import Path

import geopandas as gpd
import networkx as nx


def build_reach_graph(reaches: gpd.GeoDataFrame, basins: gpd.GeoDataFrame) -> nx.DiGraph:
    """Build a directed reach graph from stream segment endpoints."""
    reach_graph = nx.DiGraph()
    basin_lookup = {}
    if not basins.empty and "reach_id" in basins:
        basin_lookup = {row.reach_id: row.basin_id for _, row in basins.dropna(subset=["reach_id"]).iterrows()}

    for _, reach in reaches.iterrows():
        reach_graph.add_node(
            int(reach.reach_id),
            from_node=reach.from_node,
            to_node=reach.to_node,
            basin_id=basin_lookup.get(reach.reach_id),
            length=float(reach.geometry.length),
            accumulation=float(reach.get("accumulation", 0.0)),
        )

    by_from = {}
    for _, reach in reaches.iterrows():
        by_from.setdefault(reach.from_node, []).append(int(reach.reach_id))

    for _, reach in reaches.iterrows():
        for downstream_reach in by_from.get(reach.to_node, []):
            reach_graph.add_edge(int(reach.reach_id), downstream_reach)

    return reach_graph


def write_graphml(reach_graph: nx.DiGraph, path: str | Path) -> None:
    serializable = nx.DiGraph()
    for node, attrs in reach_graph.nodes(data=True):
        serializable.add_node(node, **{key: "" if value is None else value for key, value in attrs.items()})
    serializable.add_edges_from(reach_graph.edges())
    nx.write_graphml(serializable, path)


def write_json(reach_graph: nx.DiGraph, path: str | Path) -> None:
    payload = {
        "nodes": [{"id": node, **attrs} for node, attrs in reach_graph.nodes(data=True)],
        "edges": [{"source": source, "target": target} for source, target in reach_graph.edges()],
    }
    Path(path).write_text(json.dumps(payload, indent=2), encoding="utf-8")

