from __future__ import annotations

import json
from pathlib import Path

import click

from raven.pipeline import Pipeline


def _pipeline(config_path: str | Path) -> Pipeline:
    return Pipeline.from_yaml(config_path)


@click.group()
def cli() -> None:
    """RAVEN terrain-to-network processing commands."""


@cli.command()
@click.argument("config_path", type=click.Path(exists=True, dir_okay=False))
def info(config_path: str) -> None:
    """Print the parsed config."""
    pipe = _pipeline(config_path)
    click.echo(json.dumps(pipe.config, indent=2, sort_keys=True))


@cli.command()
@click.argument("config_path", type=click.Path(exists=True, dir_okay=False))
def run(config_path: str) -> None:
    """Run the complete workflow."""
    outputs = _pipeline(config_path).run()
    for label, path in outputs.items():
        click.echo(f"{label}: {path}")


@cli.command()
@click.argument("config_path", type=click.Path(exists=True, dir_okay=False))
def streams(config_path: str) -> None:
    """Run through stream extraction and report the reach count."""
    pipe = _pipeline(config_path)
    dem = pipe.condition_dem(pipe.load_dem())
    fdir = pipe.flow_direction(dem)
    facc = pipe.flow_accumulation(fdir)
    extracted = pipe.extract_streams(fdir, facc)
    click.echo(f"{len(extracted.reaches)} reaches extracted")


@cli.command()
@click.argument("config_path", type=click.Path(exists=True, dir_okay=False))
def graph(config_path: str) -> None:
    """Run through graph construction and report graph size."""
    pipe = _pipeline(config_path)
    dem = pipe.condition_dem(pipe.load_dem())
    fdir = pipe.flow_direction(dem)
    facc = pipe.flow_accumulation(fdir)
    extracted = pipe.extract_streams(fdir, facc)
    basin_collection = pipe.delineate_basins(fdir, extracted)
    reach_graph = pipe.build_graph(extracted, basin_collection)
    click.echo(f"{reach_graph.number_of_nodes()} nodes, {reach_graph.number_of_edges()} edges")


if __name__ == "__main__":
    cli()
