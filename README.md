# RAVEN

RAVEN is a small geomatics toolkit for turning a digital elevation model into a reproducible drainage-network package:

**R**eproducible **A**ccumulation, **V**ectorization, and **E**dge **N**etworks.

The project is designed for readable hydrologic terrain analysis workflows. Given a DEM and a simple YAML config, RAVEN can condition the raster, calculate D8 flow direction and accumulation, extract streams, delineate simple drainage areas, build a directed reach graph, calculate stream ordering, attach reach/basin attributes, and export quicklook figures for review.

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
cp configs/example_aoi.yml configs/my_aoi.yml
raven run configs/my_aoi.yml
```

Before running, edit `configs/my_aoi.yml` so `dem_path` points to a projected DEM. RAVEN refuses geographic CRSs for processing because cell size and area calculations need projected units.

## Repository Layout

```text
src/raven/       Python package and CLI
configs/         Example YAML configuration
docs/            Quickstart, methodology, and data-source notes
tests/           Synthetic smoke tests with no network dependency
data/            Local working data folders, ignored except .gitkeep files
outputs/         Regenerable products, ignored except .gitkeep files
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

