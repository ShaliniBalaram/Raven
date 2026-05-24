# Quickstart

Create an environment and install the package:

```bash
python -m venv .venv
source .venv/bin/activate
pip install -e .
```

Run the bundled Gore Range workflow:

```bash
raven run configs/example_aoi.yml
```

The example config uses `data/processed/gore_range_albers_250m_subset.tif`, a projected subset derived from a real Gore Range DEM hosted on Zenodo.

To re-download and rebuild that DEM-derived subset:

```bash
python scripts/prepare_gore_range_dem.py
```

For another area of interest, copy the example config:

```bash
cp configs/example_aoi.yml configs/gore_range_variant.yml
```

Then edit the copied config:

- set `name`
- set `dem_path`
- optionally set `aoi_path`
- choose a stream accumulation threshold
- keep the DEM in a projected CRS

Run the copied config:

```bash
raven run configs/gore_range_variant.yml
```

Outputs are written under `outputs/` by default.
