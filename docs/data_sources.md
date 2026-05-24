# Data Sources

RAVEN works with local DEM files. These sources are commonly used in geomatics and hydrology workflows:

- Copernicus DEM GLO-30
- NASADEM
- MERIT Hydro
- HydroSHEDS and HydroRIVERS for reference hydrography
- regional lidar DEMs from national or state mapping agencies

Always check licensing, vertical datum, spatial resolution, and hydrologic conditioning status before comparing outputs between datasets.

## Bundled Demo Data

The runnable demo uses the Gore Range GeoTIFF archive from Zenodo record `10.5281/zenodo.3940482`. The archive contains multiscale projected elevation models centered on the Gore Range, Colorado, USA. `scripts/prepare_gore_range_dem.py` extracts the 250 m Albers GeoTIFF and writes a compact high-relief subset for RAVEN processing.

Source record: https://zenodo.org/records/3940482
