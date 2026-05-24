# Methodology

RAVEN follows a compact raster-to-vector terrain workflow.

## DEM Loading

The DEM is opened with Rasterio. If an AOI is supplied, the DEM is clipped with the AOI geometry. Processing requires a projected CRS so cell area, stream length, basin area, and drainage density are meaningful.

## Conditioning

Pit filling removes local depressions that interrupt downstream routing. The built-in implementation is intentionally small for transparency. Production workflows can substitute dedicated hydrology tools such as WhiteboxTools or pysheds.

## D8 Flow

Each cell routes to the steepest downslope neighbor among eight directions. Direction codes follow a common D8 pattern:

```text
32  64 128
16   x   1
 8   4   2
```

## Accumulation

Flow accumulation counts the number of upstream cells draining through each cell. Cells above a configured threshold are treated as stream cells.

## Stream Vectorization

Thresholded stream cells are converted into short line reaches between cell centers. The reach table stores endpoint IDs so downstream graph edges can be created without spatial guessing.

## Basins

The default basin mode creates a simple basin envelope per reach. Pour-point mode can create basin envelopes around supplied gauge points after snapping to the stream network.

## Graph and Ordering

RAVEN builds a directed `networkx.DiGraph` where nodes are reaches and edges point downstream. It calculates:

- Strahler order for branching hierarchy
- Shreve magnitude for upstream link count
- a first-level Pfafstetter-style outlet grouping

## Attributes

Reach length, basin area, and drainage density are attached to graph nodes. Elevation and slope fields are reserved for richer zonal statistics in future versions.

