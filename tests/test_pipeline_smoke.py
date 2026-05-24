from __future__ import annotations

import networkx as nx

from raven import condition, flow, ordering, streams


def test_fill_flow_and_stream_smoke(tiny_dem):
    conditioned = condition.fill_pits(tiny_dem, method="simple")
    fdir = flow.flow_direction(conditioned, method="simple")
    facc = flow.flow_accumulation(fdir)
    extracted = streams.extract_streams(facc, threshold=2)

    assert conditioned.array.shape == tiny_dem.array.shape
    assert fdir.array.max() > 0
    assert facc.array.max() >= 2
    assert not extracted.reaches.empty
    assert {"reach_id", "from_node", "to_node"}.issubset(extracted.reaches.columns)


def test_strahler_and_shreve_on_toy_graph():
    graph = nx.DiGraph()
    graph.add_edges_from([(1, 3), (2, 3), (3, 4), (5, 4)])

    ordering.add_strahler_order(graph)
    ordering.add_shreve_magnitude(graph)

    assert graph.nodes[1]["strahler"] == 1
    assert graph.nodes[2]["strahler"] == 1
    assert graph.nodes[3]["strahler"] == 2
    assert graph.nodes[4]["strahler"] == 2
    assert graph.nodes[4]["shreve"] == 3

