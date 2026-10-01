"""
build_shapes.py

Rebuilds points / lines / polygons from a point-based GeoPackage where every
vertex is its own row, and reports problems found in the input.

HOW THE DATA IS STRUCTURED (as found in the GeoPackage)
  * One "layer" value per survey (site + date) per SampFormat (Line, Poly, MU).
  * PolygonNum is the shape number inside that layer.
  * PointNum is a RUNNING COUNTER shared by the whole survey (it does NOT
    restart at 1 for each shape). So shape 4 might hold points 4,5 and shape 5
    points 6,7. Gaps between shapes are normal because the Line, Poly and MU
    layers of a survey share the same counter.
  * Rows are already sorted, so each shape's rows are consecutive.

CHECKS (only the previous row is needed for the sequence checks)
  ERROR   -> definitely wrong:
    sequence_error             within a shape, PointNum repeats or goes backwards
    point_number_not_ascending first row of a new shape has a PointNum that is
                               not greater than the previous shape's last one
    missing_data               a required column is empty
    shape_type_mismatch        wrong number of points for the SampFormat
                               (Poly needs >= 3, Line >= 2, MU exactly 1)
    unknown_shape_type         SampFormat is not Line / Poly / MU
    mixed_shape_type           rows of one shape disagree on SampFormat
    shape_rows_not_contiguous  rows of one shape are split up in the file
  WARNING -> suspicious but not provably wrong:
    point_number_jump          within a shape, PointNum jumps forward by more
                               than 1 (e.g. 1 -> 20): a point may be missing
    unaccounted_point_gap      numbers skipped between two shapes do not exist
                               anywhere in that survey (possible deleted point)

OUTPUTS (written to --out-dir)
  all_shapes.gpkg      EVERY shape, built without regard to the checks. Shapes
                       whose point count doesn't suit their SampFormat are drawn
                       as the closest geometry that is possible (see
                       build_geometry). One layer per geometry type.
  flagged_shapes.gpkg  ONLY the shapes with at least one ERROR or WARNING, with
                       columns IssueLevel, IssueTypes, IssueCount, ProblemFIDs.
  errors_report.csv    One row per problem: error type, severity, FID, shape
                       number, point number, description, suggested correction.

Usage:
    python build_shapes.py INPUT.gpkg [--out-dir DIR] [--input-layer NAME]

Requires: geopandas (with pyogrio), shapely, pandas
"""

import argparse
import os
import sys

import geopandas as gpd
import pandas as pd
from shapely.geometry import LineString, MultiPoint, Point, Polygon

# ----------------------------------------------------------------------------
# CONFIG - edit here if your schema differs
# ----------------------------------------------------------------------------
LAYER_COL = "layer"            # one value per survey per SampFormat
SHAPE_COL = "PolygonNum"       # shape number within a layer
POINT_COL = "PointNum"         # running vertex counter within a survey
FORMAT_COL = "SampFormat"      # 'Line', 'Poly' or 'MU'
SURVEY_COLS = ["Site", "Day", "Month", "Year"]   # identifies one survey

# Columns that must not be empty. Attributes / Elevation / Notes are left out
# because blanks there are common; add them here if blanks should count.
REQUIRED_COLS = [
    "Site", "Day", "Month", "Year", "PolygonNum", "PointNum",
    "Sub&Att", "Subclass", "SampFormat", "Northing", "Easting",
]

# Minimum number of vertices for each SampFormat (lower-case keys)
MIN_POINTS = {"poly": 3, "line": 2, "mu": 1}
# Maximum number of vertices for each SampFormat (MU is a single point)
MAX_POINTS = {"mu": 1}

# Attribute columns carried onto each built shape (first non-null value)
ATTR_COLS = ["Sub&Att", "Subclass", "Attributes", "layer", "path"]
# Columns whose distinct values are joined together instead
JOIN_COLS = ["Notes"]

INCLUDE_Z = True               # keep the Z value of the input points

CSV_COLUMNS = [
    "error_type", "severity", "fid", "layer", "shape_number", "point_number",
    "Site", "Day", "Month", "Year", "description", "suggested_correction",
    "fids_in_shape",
]


# ----------------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------------
def is_blank(series: pd.Series) -> pd.Series:
    """True where a value is null or an empty / whitespace-only string."""
    blank = series.isna()
    if not pd.api.types.is_numeric_dtype(series):
        blank |= series.astype(str).str.strip().eq("")
    return blank


def fmt_num(v):
    """Show whole numbers without a trailing .0"""
    if pd.isna(v):
        return ""
    return str(int(v)) if float(v).is_integer() else str(v)


def prepare(gdf: gpd.GeoDataFrame) -> gpd.GeoDataFrame:
    needed = [LAYER_COL, SHAPE_COL, POINT_COL, FORMAT_COL] + SURVEY_COLS
    missing = [c for c in needed + REQUIRED_COLS if c not in gdf.columns]
    if missing:
        raise ValueError(f"Input is missing expected column(s): {sorted(set(missing))}")

    df = gdf.sort_index().copy()          # index = FID -> keep original row order
    df["_pos"] = range(len(df))
    df["_fmt"] = df[FORMAT_COL].astype("string").str.strip().str.lower()
    df["_shape"] = df[LAYER_COL].astype(str) + "|" + df[SHAPE_COL].astype(str)
    df["_survey"] = df[SURVEY_COLS].astype(str).agg("|".join, axis=1)
    return df


# ----------------------------------------------------------------------------
# Validation
# ----------------------------------------------------------------------------
def validate(df: gpd.GeoDataFrame):
    """Return a DataFrame with one row per problem found."""
    problems = []          # list of dicts, one per reported problem

    def add(row_idx, error_type, severity, description, correction):
        r = df.loc[row_idx]
        problems.append({
            "error_type": error_type,
            "severity": severity,
            "fid": row_idx,
            "layer": r[LAYER_COL],
            "shape_number": r[SHAPE_COL],
            "point_number": r[POINT_COL],
            "Site": r["Site"], "Day": r["Day"], "Month": r["Month"], "Year": r["Year"],
            "description": description,
            "suggested_correction": correction,
            "_shape": r["_shape"],
        })

    # ---- 1. missing data (row level) ------------------------------------
    for col in REQUIRED_COLS:
        for idx in df.index[is_blank(df[col])]:
            add(idx, "missing_data", "ERROR",
                f"Required column '{col}' is empty",
                f"Fill in '{col}'")
    for idx in df.index[df.geometry.isna() | df.geometry.is_empty]:
        add(idx, "missing_data", "ERROR", "Row has no geometry", "Add the point geometry")

    # ---- 2. sequence checks (compare with previous row only) ------------
    survey_points = df.groupby("_survey")[POINT_COL].apply(set).to_dict()
    prev = None
    for idx, row in df.iterrows():
        if prev is not None and row[LAYER_COL] == prev[LAYER_COL]:
            p_prev, p_cur = prev[POINT_COL], row[POINT_COL]
            if pd.notna(p_prev) and pd.notna(p_cur):
                d = p_cur - p_prev
                if row["_shape"] == prev["_shape"]:
                    if d <= 0:
                        kind = ("duplicate point number" if d == 0
                                else "point number goes backwards")
                        add(idx, "sequence_error", "ERROR",
                            f"Within shape {fmt_num(row[SHAPE_COL])}: PointNum "
                            f"{fmt_num(p_cur)} follows {fmt_num(p_prev)} (FID {prev.name}) - {kind}",
                            f"PointNum should be {fmt_num(p_prev + 1)}")
                    elif d > 1:
                        add(idx, "point_number_jump", "WARNING",
                            f"Within shape {fmt_num(row[SHAPE_COL])}: PointNum jumps "
                            f"{fmt_num(p_prev)} -> {fmt_num(p_cur)} (FID {prev.name}), "
                            f"skipping {fmt_num(d - 1)} number(s)",
                            f"Check for a missing point; if none is missing, "
                            f"PointNum should be {fmt_num(p_prev + 1)}")
                else:   # first row of a new shape
                    if d <= 0:
                        add(idx, "point_number_not_ascending", "ERROR",
                            f"New shape {fmt_num(row[SHAPE_COL])} starts at PointNum "
                            f"{fmt_num(p_cur)} but previous shape "
                            f"{fmt_num(prev[SHAPE_COL])} ended at {fmt_num(p_prev)} (FID {prev.name})",
                            f"PointNum should be greater than {fmt_num(p_prev)} "
                            f"(or previous shape is misnumbered)")
                    elif d > 1:
                        used = survey_points[row["_survey"]]
                        gap = [n for n in range(int(p_prev) + 1, int(p_cur)) if n not in used]
                        if gap:
                            shown = ", ".join(str(n) for n in gap[:10]) + (" ..." if len(gap) > 10 else "")
                            add(idx, "unaccounted_point_gap", "WARNING",
                                f"PointNum jumps {fmt_num(p_prev)} -> {fmt_num(p_cur)} between shapes "
                                f"{fmt_num(prev[SHAPE_COL])} and {fmt_num(row[SHAPE_COL])}; "
                                f"number(s) {shown} not used anywhere in this survey",
                                "Check for a deleted point, or renumber if PointNum was mistyped")
        prev = row

    # ---- 3. shape-level checks ------------------------------------------
    for key, g in df.groupby("_shape", sort=False):
        first = g.index[0]
        n = len(g)

        # rows of one shape must sit next to each other in the file
        if g["_pos"].max() - g["_pos"].min() + 1 != n:
            add(first, "shape_rows_not_contiguous", "ERROR",
                f"Rows of shape {fmt_num(g[SHAPE_COL].iloc[0])} are split up in the file "
                f"(spans positions {g['_pos'].min()}-{g['_pos'].max()} but has {n} rows)",
                "Check that all rows of this shape have the correct shape number")

        fmts = g["_fmt"].dropna().unique()
        if len(fmts) > 1:
            add(first, "mixed_shape_type", "ERROR",
                f"Rows of this shape have different SampFormat values: {sorted(fmts)}",
                "Make SampFormat consistent for the shape")
        elif len(fmts) == 1:
            fmt = fmts[0]
            if fmt not in MIN_POINTS:
                add(first, "unknown_shape_type", "ERROR",
                    f"SampFormat '{g[FORMAT_COL].iloc[0]}' is not Line, Poly or MU",
                    "Set SampFormat to Line, Poly or MU")
            elif n < MIN_POINTS[fmt]:
                label = g[FORMAT_COL].iloc[0]
                options = {"poly": "add missing point(s), or reclassify as Line" + (" / MU" if n == 1 else ""),
                           "line": "add the missing point, or reclassify as MU"}[fmt]
                add(first, "shape_type_mismatch", "ERROR",
                    f"Classified as {label} (needs >= {MIN_POINTS[fmt]} points) "
                    f"but shape {fmt_num(g[SHAPE_COL].iloc[0])} has {n} row(s)",
                    f"Either {options}")
            elif n > MAX_POINTS.get(fmt, n):
                add(first, "shape_type_mismatch", "ERROR",
                    f"Classified as {g[FORMAT_COL].iloc[0]} (must be exactly "
                    f"{MAX_POINTS[fmt]} point) but shape {fmt_num(g[SHAPE_COL].iloc[0])} "
                    f"has {n} rows",
                    "Either reclassify as Line (or Poly if it should be closed), "
                    "or split into separate single-point shapes with their own shape numbers")

    errors = pd.DataFrame(problems)
    if errors.empty:
        errors = pd.DataFrame(columns=CSV_COLUMNS + ["_shape"])
    return errors


# ----------------------------------------------------------------------------
# Geometry building
# ----------------------------------------------------------------------------
def coords_of(group: pd.DataFrame):
    if INCLUDE_Z:
        return [(p.x, p.y, p.z) if p.has_z else (p.x, p.y) for p in group.geometry]
    return [(p.x, p.y) for p in group.geometry]


def build_geometry(group: pd.DataFrame):
    """Build one shape from its rows (already in ascending PointNum order).

    Normal cases follow SampFormat: Poly -> Polygon, Line -> LineString,
    MU -> Point. Shapes with a point count that doesn't suit their SampFormat
    still get the closest geometry that can exist, so nothing is dropped:
        1 point (any format)        -> Point
        Poly with 2 points          -> LineString
        MU with several points      -> MultiPoint
        unknown / mixed SampFormat  -> most common format in the shape is used;
                                       if there is none, 2+ points -> LineString
    """
    coords = coords_of(group)
    if len(coords) == 1:
        return Point(coords[0])

    fmts = group["_fmt"].dropna()
    fmt = fmts.mode().iloc[0] if not fmts.empty else None

    if fmt == "poly":
        return Polygon(coords) if len(coords) >= 3 else LineString(coords)
    if fmt == "mu":
        return MultiPoint(coords)
    return LineString(coords)


def shape_row(g: pd.DataFrame, geom) -> dict:
    first = g.iloc[0]
    row = {c: first[c] for c in ["Site", "Day", "Month", "Year", SHAPE_COL, FORMAT_COL]}
    row["NumPoints"] = len(g)
    row["GeomType"] = geom.geom_type
    row["MinPointNum"] = g[POINT_COL].min()
    row["MaxPointNum"] = g[POINT_COL].max()
    row["SourceFIDs"] = ", ".join(str(i) for i in g.index)
    for col in ATTR_COLS:
        if col in g.columns:
            nn = g[col].dropna()
            row[col] = nn.iloc[0] if not nn.empty else None
    for col in JOIN_COLS:
        if col in g.columns:
            vals = list(dict.fromkeys(str(v) for v in g[col].dropna()))
            row[col] = "; ".join(vals) if vals else None
    return row


def build_shapes(df: gpd.GeoDataFrame, issues: pd.DataFrame = None, only_flagged=False):
    """Build every shape (or, with only_flagged, just those with a problem).

    When `issues` is given, flag columns are added to each shape.
    """
    flagged = {}
    if issues is not None and len(issues):
        for key, g in issues.groupby("_shape"):
            flagged[key] = {
                "IssueLevel": "ERROR" if (g["severity"] == "ERROR").any() else "WARNING",
                "IssueTypes": ", ".join(sorted(g["error_type"].unique())),
                "IssueCount": len(g),
                "ProblemFIDs": ", ".join(str(i) for i in sorted(g["fid"].unique())),
            }

    records = []
    for key, g in df.groupby("_shape", sort=False):
        if only_flagged and key not in flagged:
            continue
        g = g.sort_values(POINT_COL, kind="mergesort", na_position="last")
        geom = build_geometry(g)
        row = shape_row(g, geom)
        if only_flagged:
            row.update(flagged[key])
        row["geometry"] = geom
        records.append(row)
    return gpd.GeoDataFrame(records, geometry="geometry", crs=df.crs)


def write_layers(shapes: gpd.GeoDataFrame, path: str, prefix: str):
    names = {"Point": "points", "MultiPoint": "multipoints",
             "LineString": "lines", "Polygon": "polygons"}
    first = True
    for gtype, name in names.items():
        sub = shapes[shapes["GeomType"] == gtype]
        if len(sub):
            sub.to_file(path, layer=f"{prefix}_{name}", driver="GPKG",
                        mode="w" if first else "a")
            first = False
            print(f"  {path}: {len(sub)} {gtype} shape(s) -> layer '{prefix}_{name}'")


# ----------------------------------------------------------------------------
# Main
# ----------------------------------------------------------------------------
def main():
    ap = argparse.ArgumentParser(description="Validate and rebuild shapes from a labeled point GeoPackage")
    ap.add_argument("input_gpkg")
    ap.add_argument("--out-dir", default=".", help="Folder for the three output files (default: current folder)")
    ap.add_argument("--input-layer", default=None)
    args = ap.parse_args()

    os.makedirs(args.out_dir, exist_ok=True)
    all_path = os.path.join(args.out_dir, "all_shapes.gpkg")
    flag_path = os.path.join(args.out_dir, "flagged_shapes.gpkg")
    csv_path = os.path.join(args.out_dir, "errors_report.csv")
    for p in (all_path, flag_path):
        if os.path.exists(p):
            os.remove(p)

    raw = gpd.read_file(args.input_gpkg, layer=args.input_layer, fid_as_index=True)
    print(f"Read {len(raw)} vertex rows from {args.input_gpkg}")
    df = prepare(raw)

    issues = validate(df)
    fids = df.groupby("_shape", sort=False).apply(
        lambda g: ", ".join(str(i) for i in g.index), include_groups=False)
    issues["fids_in_shape"] = issues["_shape"].map(fids)

    report = issues[CSV_COLUMNS].sort_values(["fid", "error_type"]).reset_index(drop=True)
    report.to_csv(csv_path, index=False)

    all_shapes = build_shapes(df)
    flagged = build_shapes(df, issues, only_flagged=True)

    print(f"{len(all_shapes)} shapes in total, {len(flagged)} with at least one error/warning")
    print(f"{len(report)} problem(s) logged -> {csv_path}")
    if len(report):
        print(report.groupby(["error_type", "severity"]).size().to_string())
    if len(flagged):
        print("Flagged shapes by level:", flagged["IssueLevel"].value_counts().to_dict())

    write_layers(all_shapes, all_path, "all")
    if len(flagged):
        write_layers(flagged, flag_path, "flagged")


if __name__ == "__main__":
    sys.exit(main())
