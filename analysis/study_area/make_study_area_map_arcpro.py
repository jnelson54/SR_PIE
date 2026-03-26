"""Build a Plum Island Estuary study-area map in ArcGIS Pro.

Run this script from ArcGIS Pro's Python environment.

Default behavior:
- opens the current ArcGIS Pro project
- creates or reuses a map named "PIE Study Area"
- creates feature classes for Gulf water, estuary water, region boxes, and labels
- adds the local 25k coastline shapefile and the generated layers to the map

This script is intended to create a clean starting point in ArcGIS Pro.
Final layout tweaks are still easiest to do interactively in Pro.
"""

from __future__ import annotations

import os
from pathlib import Path

import arcpy


MAP_NAME = "PIE Study Area"
WGS84 = arcpy.SpatialReference(4326)
EXTENT = (-70.945, 42.664, -70.742, 42.807)
DEFAULT_REPO_HINTS = [
    Path(r"c:\Users\jn03823\University of Georgia\Nelson Lab UGA Marine Science-Jimmy Nelson - Jimmy Nelson\Manuscripts\NCC_PIE\NatureComms_Analysis_Repository_2026-03-08_v2"),
]


def is_repo_root(path: Path) -> bool:
    return (
        path.is_dir()
        and (path / "analysis" / "study_area").exists()
        and (path / "analysis" / "common").exists()
        and (path / "figures").exists()
    )


def find_repo_root(start: Path, max_depth: int = 3) -> Path | None:
    start = start.resolve()

    for candidate in [start, *start.parents]:
        if is_repo_root(candidate):
            return candidate

    for root, dirnames, _ in os.walk(start):
        root_path = Path(root)
        depth = len(root_path.relative_to(start).parts)
        if depth > max_depth:
            dirnames[:] = []
            continue
        if is_repo_root(root_path):
            return root_path

    return None


def repo_root(repo_hint: str | Path | None = None) -> Path:
    candidate_paths: list[Path] = []

    if repo_hint:
        candidate_paths.append(Path(repo_hint))

    env_repo_root = os.environ.get("PIE_REPO_ROOT")
    if env_repo_root:
        candidate_paths.append(Path(env_repo_root))

    candidate_paths.extend(DEFAULT_REPO_HINTS)

    script_file = globals().get("__file__")
    if script_file:
        candidate_paths.append(Path(script_file).resolve().parent)

    try:
        aprx = arcpy.mp.ArcGISProject("CURRENT")
        if aprx.homeFolder:
            candidate_paths.append(Path(aprx.homeFolder))
        if aprx.filePath:
            candidate_paths.append(Path(aprx.filePath).resolve().parent)
    except Exception:
        pass

    candidate_paths.append(Path.cwd())

    for candidate in candidate_paths:
        match = find_repo_root(candidate)
        if match is not None:
            return match

    raise FileNotFoundError(
        "Could not determine repository root. Pass repo_hint=..., set PIE_REPO_ROOT, run this script from the repo, "
        "or open an ArcGIS Pro project whose home folder is inside the repository."
    )


def first_existing_dir(paths: list[Path]) -> Path:
    for path in paths:
        if path.exists():
            return path
    raise FileNotFoundError("Could not find required spatial inputs for ArcGIS Pro map build.")


def ensure_output_gdb(root: Path) -> Path:
    output_dir = root / "analysis" / "study_area" / "arcpro_output"
    output_dir.mkdir(parents=True, exist_ok=True)
    gdb = output_dir / "pie_study_area_map.gdb"
    if not gdb.exists():
        arcpy.management.CreateFileGDB(str(output_dir), gdb.name)
    return gdb


def delete_if_exists(path: Path) -> None:
    if arcpy.Exists(str(path)):
        arcpy.management.Delete(str(path))


def polygon(points: list[tuple[float, float]]) -> arcpy.Polygon:
    array = arcpy.Array([arcpy.Point(x, y) for x, y in points])
    return arcpy.Polygon(array, WGS84)


def rectangle(xmin: float, ymin: float, xmax: float, ymax: float) -> arcpy.Polygon:
    return polygon(
        [
            (xmin, ymin),
            (xmax, ymin),
            (xmax, ymax),
            (xmin, ymax),
            (xmin, ymin),
        ]
    )


def create_polygon_fc(path: Path, field_name: str, features: list[tuple[str, arcpy.Polygon]]) -> Path:
    delete_if_exists(path)
    arcpy.management.CreateFeatureclass(str(path.parent), path.name, "POLYGON", spatial_reference=WGS84)
    arcpy.management.AddField(str(path), field_name, "TEXT", field_length=64)
    with arcpy.da.InsertCursor(str(path), ["SHAPE@", field_name]) as cursor:
        for name, geometry in features:
            cursor.insertRow([geometry, name])
    return path


def create_point_fc(path: Path, features: list[tuple[str, float, float]]) -> Path:
    delete_if_exists(path)
    arcpy.management.CreateFeatureclass(str(path.parent), path.name, "POINT", spatial_reference=WGS84)
    arcpy.management.AddField(str(path), "LABEL", "TEXT", field_length=128)
    with arcpy.da.InsertCursor(str(path), ["SHAPE@XY", "LABEL"]) as cursor:
        for label, x, y in features:
            cursor.insertRow([(x, y), label])
    return path


def list_maps(aprx: arcpy.mp.ArcGISProject) -> list[arcpy.mp.Map]:
    return aprx.listMaps(MAP_NAME)


def get_or_create_map(aprx: arcpy.mp.ArcGISProject) -> arcpy.mp.Map:
    matches = list_maps(aprx)
    if matches:
        return matches[0]
    return aprx.createMap(MAP_NAME, "MAP")


def remove_existing_layer(map_obj: arcpy.mp.Map, layer_name: str) -> None:
    for layer in map_obj.listLayers():
        if layer.name == layer_name:
            map_obj.removeLayer(layer)


def add_layer(map_obj: arcpy.mp.Map, layer_path: Path, layer_name: str):
    remove_existing_layer(map_obj, layer_name)
    layer = map_obj.addDataFromPath(str(layer_path))
    return layer


def try_set_simple_fill(layer, rgb: tuple[int, int, int], outline_rgb: tuple[int, int, int] | None = None, outline_width: float = 0.8):
    try:
        sym = layer.symbology
        sym.updateRenderer("SimpleRenderer")
        sym.renderer.symbol.color = {"RGB": [rgb[0], rgb[1], rgb[2], 100]}
        if outline_rgb is not None:
            sym.renderer.symbol.outlineColor = {"RGB": [outline_rgb[0], outline_rgb[1], outline_rgb[2], 100]}
            sym.renderer.symbol.outlineWidth = outline_width
        else:
            sym.renderer.symbol.outlineColor = {"RGB": [0, 0, 0, 0]}
        layer.symbology = sym
    except Exception:
        pass


def try_set_simple_line(layer, rgb: tuple[int, int, int], width: float = 1.0):
    try:
        sym = layer.symbology
        sym.updateRenderer("SimpleRenderer")
        sym.renderer.symbol.color = {"RGB": [rgb[0], rgb[1], rgb[2], 100]}
        sym.renderer.symbol.width = width
        layer.symbology = sym
    except Exception:
        pass


def try_enable_labels(layer, expression: str, size: int = 12, bold: bool = False):
    try:
        layer.showLabels = True
        label_class = layer.listLabelClasses()[0]
        label_class.expression = expression
        label_class.visible = True
        symbol = label_class.textSymbol
        symbol.size = size
        if bold:
            symbol.fontStyle = "Bold"
        label_class.textSymbol = symbol
    except Exception:
        pass


def build_inputs(root: Path) -> tuple[Path, Path, Path, Path, Path]:
    coast_dir = first_existing_dir(
        [
            root / "analysis" / "study_area" / "data" / "coast",
            root.parents[2] / "PIE_Mixing_Model_2020" / "PIE_Fiddler_Tethering" / "mass_maps",
        ]
    )
    coast_path = coast_dir / "COAST25K_ARC.shp"

    gdb = ensure_output_gdb(root)
    gulf_fc = gdb / "gulf_water"
    sound_fc = gdb / "sound_water"
    region_fc = gdb / "region_boxes"
    label_fc = gdb / "map_labels"

    gulf_points = [
        (-70.815, 42.807),
        (-70.742, 42.807),
        (-70.742, 42.664),
        (-70.754, 42.664),
        (-70.764, 42.690),
        (-70.775, 42.714),
        (-70.788, 42.744),
        (-70.800, 42.778),
        (-70.815, 42.807),
    ]
    sound_points = [
        (-70.925, 42.752),
        (-70.895, 42.764),
        (-70.865, 42.765),
        (-70.835, 42.759),
        (-70.807, 42.747),
        (-70.784, 42.731),
        (-70.768, 42.710),
        (-70.758, 42.692),
        (-70.764, 42.684),
        (-70.778, 42.688),
        (-70.796, 42.699),
        (-70.820, 42.715),
        (-70.848, 42.728),
        (-70.878, 42.737),
        (-70.908, 42.742),
        (-70.925, 42.752),
    ]
    regions = [
        ("Upper", rectangle(-70.894, 42.742, -70.848, 42.761)),
        ("Middle", rectangle(-70.825, 42.721, -70.782, 42.749)),
        ("Lower", rectangle(-70.807, 42.692, -70.765, 42.716)),
    ]
    labels = [
        ("Upper", -70.871, 42.7635),
        ("Middle", -70.798, 42.752),
        ("Lower", -70.775, 42.7175),
        ("Plum Island Sound", -70.813, 42.729),
        ("Gulf of Maine", -70.747, 42.720),
    ]

    create_polygon_fc(gulf_fc, "NAME", [("Gulf of Maine", polygon(gulf_points))])
    create_polygon_fc(sound_fc, "NAME", [("Plum Island Sound", polygon(sound_points))])
    create_polygon_fc(region_fc, "REGION", regions)
    create_point_fc(label_fc, labels)

    return coast_path, gulf_fc, sound_fc, region_fc, label_fc


def main(aprx_path: str = "CURRENT", repo_hint: str | Path | None = None) -> None:
    root = repo_root(repo_hint=repo_hint)
    coast_path, gulf_fc, sound_fc, region_fc, label_fc = build_inputs(root)

    aprx = arcpy.mp.ArcGISProject(aprx_path)
    map_obj = get_or_create_map(aprx)

    coast_layer = add_layer(map_obj, coast_path, "COAST25K_ARC")
    gulf_layer = add_layer(map_obj, gulf_fc, "gulf_water")
    sound_layer = add_layer(map_obj, sound_fc, "sound_water")
    region_layer = add_layer(map_obj, region_fc, "region_boxes")
    label_layer = add_layer(map_obj, label_fc, "map_labels")

    try_set_simple_fill(gulf_layer, (207, 225, 236))
    try_set_simple_fill(sound_layer, (220, 234, 242))
    try_set_simple_fill(region_layer, (0, 0, 0), outline_rgb=(35, 35, 35), outline_width=1.2)
    try_set_simple_line(coast_layer, (42, 42, 42), width=1.1)
    try_enable_labels(label_layer, "$feature.LABEL", size=12, bold=False)

    try:
        if aprx_path != "CURRENT":
            aprx.save()
    except Exception:
        pass

    arcpy.AddMessage(f"Prepared ArcGIS Pro map: {MAP_NAME}")
    arcpy.AddMessage(f"Coastline source: {coast_path}")
    arcpy.AddMessage(f"Generated workspace: {ensure_output_gdb(root)}")


if __name__ == "__main__":
    main()