from __future__ import annotations

import networkx as nx


def add_all_orderings(graph: nx.DiGraph) -> None:
    add_strahler_order(graph)
    add_shreve_magnitude(graph)
    add_pfafstetter_l1(graph)


def add_strahler_order(graph: nx.DiGraph) -> None:
    for node in nx.topological_sort(graph):
        upstream = list(graph.predecessors(node))
        if not upstream:
            graph.nodes[node]["strahler"] = 1
            continue
        orders = [graph.nodes[parent]["strahler"] for parent in upstream]
        max_order = max(orders)
        graph.nodes[node]["strahler"] = max_order + 1 if orders.count(max_order) >= 2 else max_order


def add_shreve_magnitude(graph: nx.DiGraph) -> None:
    for node in nx.topological_sort(graph):
        upstream = list(graph.predecessors(node))
        graph.nodes[node]["shreve"] = 1 if not upstream else sum(graph.nodes[parent]["shreve"] for parent in upstream)


def add_pfafstetter_l1(graph: nx.DiGraph) -> None:
    outlets = [node for node in graph.nodes if graph.out_degree(node) == 0]
    ranked = sorted(outlets, key=lambda node: graph.nodes[node].get("shreve", 1), reverse=True)
    outlet_codes = {node: str((idx + 1) * 2) for idx, node in enumerate(ranked[:4])}

    for outlet in outlets:
        code = outlet_codes.get(outlet, "9")
        graph.nodes[outlet]["pfaf_l1"] = code
        for upstream in nx.ancestors(graph, outlet):
            graph.nodes[upstream]["pfaf_l1"] = code

