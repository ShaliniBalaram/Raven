# Data Manifest

This repository keeps a compact, runnable set of geospatial inputs and outputs. Larger raw downloads remain ignored.

## Raster Data

- `data/processed/gore_range_albers_250m_subset.tif`
  - Format: GeoTIFF raster.
  - Purpose: projected DEM input for the bundled RAVEN workflow.
  - Source: derived from the Gore Range GeoTIFF archive, Zenodo record `10.5281/zenodo.3940482`.
  - CRS: NAD83 Albers equal-area projection in metres.

## Vector Data

RAVEN stores vector outputs as GeoPackage (`.gpkg`) instead of ESRI Shapefile (`.shp`). Both are vector GIS formats, but GeoPackage is a single SQLite-based file, preserves field names more reliably, supports modern CRS metadata, and avoids shapefile sidecars such as `.shx`, `.dbf`, `.prj`, and `.cpg`.

- `outputs/streams/gore_range_streams.gpkg`
  - Format: GeoPackage vector line layer.
  - Purpose: RAVEN-extracted stream reaches.

- `outputs/basins/gore_range_basins.gpkg`
  - Format: GeoPackage vector polygon layer.
  - Purpose: raster-derived drainage areas associated with extracted stream reaches.

- `data/reference/usgs_nhd_gore_flowlines.gpkg`
  - Format: GeoPackage vector line layer.
  - Purpose: independent reference hydrography for validation.
  - Source: USGS NHD ArcGIS REST service queried over the Gore Range DEM extent.

## Graph and Figure Outputs

- `outputs/graphs/gore_range_graph.graphml`
  - Format: GraphML network file.
  - Purpose: directed reach graph for graph/GIS tools.

- `outputs/graphs/gore_range_graph.json`
  - Format: JSON.
  - Purpose: readable graph export for scripts and review.

- `outputs/figures/gore_range_quicklook.png`
  - Format: PNG.
  - Purpose: visual QA overview of DEM, flow accumulation, streams, basins, and summary counts.

- `outputs/validation/stream_validation.json`
  - Format: JSON.
  - Purpose: validation metrics comparing RAVEN streams with USGS NHD reference flowlines.

- `outputs/validation/stream_validation.png`
  - Format: PNG.
  - Purpose: visual validation overlay of RAVEN streams and reference hydrography.

## Empty Keep Files

`.gitkeep` files are included only so otherwise-empty data/output directories remain visible in Git. They are not analysis inputs.
