# RAVEN

RAVEN is a small geomatics toolkit for turning a digital elevation model into a reproducible drainage-network package:

**R**eproducible **A**ccumulation, **V**ectorization, and **E**dge **N**etworks.

The project is designed for readable hydrologic terrain analysis workflows. Given a DEM and a simple YAML config, RAVEN can condition the raster, calculate D8 flow direction and accumulation, extract streams, delineate simple drainage areas, build a directed reach graph, calculate stream ordering, attach reach/basin attributes, and export quicklook figures for review.

## About

RAVEN is a portfolio-scale geomatics project focused on terrain analysis, raster processing, vector hydrography, and graph-based stream networks. It is intentionally compact: the code is large enough to show the full workflow, but small enough for a reviewer to read in one sitting.

The bundled demo uses a real DEM from Zenodo, prepared as a small projected subset so the workflow can run quickly in a repository checkout.

## What It Produces

- conditioned DEM and flow rasters
- vector stream reaches as GeoPackage layers
- basin polygons as GeoPackage layers
- directed reach graphs as GraphML and JSON
- reach ordering attributes including Strahler and Shreve
- a six-panel quicklook PNG for visual QA

## Quickstart

```bash
python -m venv .venv
source .venv/bin/activate
pip install -e .
raven run configs/example_aoi.yml
```

The example config points to `data/processed/zenodo_7797361_dem_utm50n.tif`, a projected subset derived from `DEM.tif` in Zenodo record `10.5281/zenodo.7797361`. For real work, copy `configs/example_aoi.yml`, point `dem_path` to a projected DEM, and tune the stream threshold for the area of interest. RAVEN refuses geographic CRSs for processing because cell size and area calculations need projected units.

To re-download and regenerate the bundled Zenodo-derived DEM:

```bash
python scripts/prepare_zenodo_dem.py
```

## Repository Layout

```text
src/raven/       Python package and CLI
configs/         Example YAML configuration
docs/            Quickstart, methodology, and data-source notes
tests/           Tiny smoke tests with no network dependency
data/            Zenodo DEM subset plus ignored local working folders
outputs/         Zenodo-derived outputs plus ignored regenerable products
```

## Command Line

```bash
raven info configs/example_aoi.yml
raven run configs/example_aoi.yml
raven streams configs/example_aoi.yml
raven graph configs/example_aoi.yml
```

The CLI is intentionally thin. Most behavior lives in `raven.Pipeline`, so the same workflow can be used from scripts or notebooks.

## Development

```bash
pip install -e ".[dev]"
pytest
```

The tests use toy rasters and toy graphs so they run quickly and do not need external data downloads.
