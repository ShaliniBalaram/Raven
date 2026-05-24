# Quickstart

Create an environment and install the package:

```bash
python -m venv .venv
source .venv/bin/activate
pip install -e .
```

Copy the example config:

```bash
cp configs/example_aoi.yml configs/my_aoi.yml
```

Edit `configs/my_aoi.yml`:

- set `name`
- set `dem_path`
- optionally set `aoi_path`
- choose a stream accumulation threshold
- keep the DEM in a projected CRS

Run:

```bash
raven run configs/my_aoi.yml
```

Outputs are written under `outputs/` by default.

