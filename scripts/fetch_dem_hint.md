# DEM Data Notes

RAVEN expects a local projected DEM GeoTIFF. It does not download DEMs directly because public elevation portals often change authentication, rate limits, and tile packaging.

Good starting sources:

- Copernicus DEM GLO-30 for global 30 m elevation.
- NASADEM for SRTM-derived global elevation.
- MERIT Hydro for hydrologically adjusted global terrain products.
- Local or national mapping agencies for higher-resolution lidar-derived DEMs.

Recommended preparation:

1. Download the DEM tile or mosaic for the area of interest.
2. Reproject to a suitable local projected CRS.
3. Clip to a manageable AOI.
4. Save as `data/raw/<aoi>_dem.tif`.
5. Point `configs/<aoi>.yml` to that file.

