# Validation

RAVEN outputs should be checked against independent hydrography before they are treated as analysis-ready. The repository includes a validation script for comparing extracted stream reaches with a reference stream layer.

Recommended reference sources:

- USGS National Hydrography Dataset / NHDPlus HR flowlines
- USGS 3D Hydrography Program flow network products
- state hydrography layers, when they are newer or more detailed for the area of interest

The repository includes a clipped USGS NHD reference layer for the Gore Range demo at `data/reference/usgs_nhd_gore_flowlines.gpkg`. It was queried from the USGS NHD ArcGIS REST service over the DEM extent.

Run the bundled validation:

```bash
python scripts/validate_streams.py \
  --reference-streams data/reference/usgs_nhd_gore_flowlines.gpkg \
  --tolerance 500
```

Run the same workflow with another reference stream file:

```bash
python scripts/validate_streams.py \
  --reference-streams path/to/reference_flowlines.gpkg \
  --tolerance 250
```

The script writes:

- `outputs/validation/stream_validation.json`
- `outputs/validation/stream_validation.png`

Reported metrics include total length ratio, nearest-reference distances, matched stream length within a tolerance buffer, and a visual overlay of RAVEN streams against the reference layer.

Validation should be interpreted with the DEM scale in mind. The bundled Gore Range demo uses a 250 m subset, so small headwater channels and exact line placement should not be expected to match a high-resolution reference layer perfectly.

Current bundled validation summary:

- RAVEN reaches: 70
- USGS NHD reference features after clipping: 1,974
- median nearest-reference distance: 136.42 m
- matched RAVEN stream length within 500 m: 82.71%
- RAVEN/reference total length ratio: 0.16
