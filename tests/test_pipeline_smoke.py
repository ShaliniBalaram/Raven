from __future__ import annotations

import networkx as nx
import geopandas as gpd
from shapely.geometry import LineString

from raven import condition, flow, ordering, streams
from raven.validation import validate_streams


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


def test_strahler_and_shreve_on_minimal_graph():
    graph = nx.DiGraph()
    graph.add_edges_from([(1, 3), (2, 3), (3, 4), (5, 4)])

    ordering.add_strahler_order(graph)
    ordering.add_shreve_magnitude(graph)

    assert graph.nodes[1]["strahler"] == 1
    assert graph.nodes[2]["strahler"] == 1
    assert graph.nodes[3]["strahler"] == 2
    assert graph.nodes[4]["strahler"] == 2
    assert graph.nodes[4]["shreve"] == 3


def test_stream_validation_outputs_metrics(tmp_path, tiny_dem):
    raven_path = tmp_path / "raven.gpkg"
    reference_path = tmp_path / "reference.gpkg"
    dem_path = tmp_path / "dem.tif"

    gpd.GeoDataFrame(
        [{"id": 1, "geometry": LineString([(5, -5), (35, -35)])}],
        geometry="geometry",
        crs=tiny_dem.crs,
    ).to_file(raven_path, driver="GPKG")
    gpd.GeoDataFrame(
        [{"id": 1, "geometry": LineString([(5, -6), (35, -36)])}],
        geometry="geometry",
        crs=tiny_dem.crs,
    ).to_file(reference_path, driver="GPKG")

    import rasterio

    with rasterio.open(dem_path, "w", **tiny_dem.profile) as dst:
        dst.write(tiny_dem.array, 1)

    metrics = validate_streams(raven_path, reference_path, dem_path, tmp_path / "validation", tolerance=5)

    assert metrics["raven_reach_count"] == 1
    assert metrics["reference_feature_count"] == 1
    assert metrics["matched_length_fraction"] == 1.0
    assert (tmp_path / "validation" / "stream_validation.json").exists()
    assert (tmp_path / "validation" / "stream_validation.png").exists()
