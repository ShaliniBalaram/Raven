# Quickstart

Create an environment and install the package:

```bash
python -m venv .venv
source .venv/bin/activate
pip install -e .
```

Run the bundled sample:

```bash
raven run configs/example_aoi.yml
```

The example config uses `data/processed/zenodo_7797361_dem_utm50n.tif`, a projected subset derived from a real Zenodo DEM.

To re-download and rebuild that DEM-derived subset:

```bash
python scripts/prepare_zenodo_dem.py
```

For your own area of interest, copy the example config:

```bash
cp configs/example_aoi.yml configs/my_aoi.yml
```

Then edit `configs/my_aoi.yml`:

- set `name`
- set `dem_path`
- optionally set `aoi_path`
- choose a stream accumulation threshold
- keep the DEM in a projected CRS

Run your config:

```bash
raven run configs/my_aoi.yml
```

Outputs are written under `outputs/` by default.
