from __future__ import annotations

import argparse
import json

from raven.validation import validate_streams


def main() -> None:
    parser = argparse.ArgumentParser(description="Compare RAVEN streams against reference hydrography.")
    parser.add_argument("--raven-streams", default="outputs/streams/gore_range_streams.gpkg")
    parser.add_argument("--reference-streams", required=True)
    parser.add_argument("--dem", default="data/processed/gore_range_albers_250m_subset.tif")
    parser.add_argument("--output-dir", default="outputs/validation")
    parser.add_argument("--tolerance", type=float, default=250.0)
    args = parser.parse_args()

    metrics = validate_streams(
        raven_streams_path=args.raven_streams,
        reference_streams_path=args.reference_streams,
        dem_path=args.dem,
        output_dir=args.output_dir,
        tolerance=args.tolerance,
    )
    print(json.dumps(metrics, indent=2))


if __name__ == "__main__":
    main()
