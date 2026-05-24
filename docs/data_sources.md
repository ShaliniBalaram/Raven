# Data Sources

RAVEN works with local DEM files. These sources are commonly used in geomatics and hydrology workflows:

- Copernicus DEM GLO-30
- NASADEM
- MERIT Hydro
- HydroSHEDS and HydroRIVERS for reference hydrography
- regional lidar DEMs from national or state mapping agencies

Always check licensing, vertical datum, spatial resolution, and hydrologic conditioning status before comparing outputs between datasets.

## Bundled Demo Data

The runnable demo uses `DEM.tif` from Zenodo record `10.5281/zenodo.7797361`, published as a dataset for an integrated socio-hydrological modeling framework. The original raster is in EPSG:4326, so `scripts/prepare_zenodo_dem.py` creates a compact projected subset in EPSG:32650 for RAVEN processing.

Source record: https://zenodo.org/records/7797361
