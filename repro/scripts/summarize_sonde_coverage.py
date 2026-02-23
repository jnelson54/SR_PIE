#!/usr/bin/env python3
"""Summarize sonde package coverage, sites, years, and time gaps."""

from __future__ import annotations

import argparse
import re
from pathlib import Path

import numpy as np
import pandas as pd

EXCLUDE_SITE_CODES = {"RR", "RO", "RT1A"}


def parse_package_id(path: Path) -> str:
    return path.name


def infer_site_code(file_name: str, cols: list[str], sample: pd.DataFrame) -> str:
    fn = file_name.upper()
    m = re.search(r"_YSI_([A-Z0-9]+)\.CSV$", fn)
    if m:
        return m.group(1)

    m = re.search(r"MON-PR-MBYSI", fn)
    if m:
        return "MB"
    m = re.search(r"MON-PR-RRYSI", fn)
    if m:
        return "RR"
    m = re.search(r"MON-SO-IBYCYSI", fn)
    if m:
        return "IBYC"
    m = re.search(r"MON-PR-RT1AYSI", fn)
    if m:
        return "RT1A"

    if "Station" in sample.columns:
        stations = sample["Station"].dropna().astype(str)
        if not stations.empty:
            s = stations.iloc[0].upper()
            if "EST-RO" in s:
                return "RO"
            if "EST-PR" in s:
                return "PR"
    return "UNKNOWN"


def infer_site_name(code: str) -> str:
    mapping = {
        "IB": "Ipswich Bay / lower estuary buoy",
        "IBYC": "Ipswich Bay Yacht Club (lower estuary)",
        "MR": "Middle Road Bridge (upper Parker)",
        "MB": "Middle Road Bridge (upper Parker)",
        "RR": "Railroad Bridge (mid Parker)",
        "RO": "Rowley estuary transect/sonde reach",
        "RT1A": "Route 1A bridge (Parker)",
        "PR": "Parker River transect stations",
    }
    return mapping.get(code, code)


def infer_datetime_series(df: pd.DataFrame) -> pd.Series:
    cols = [c for c in df.columns]
    lower = {c.lower(): c for c in cols}

    # Preferred explicit timestamp-like columns.
    for key in [
        "timestamp",
        "datetime",
        "date_time",
        "sampledatetime",
        "dateandtime",
    ]:
        if key in lower:
            s = pd.to_datetime(df[lower[key]], errors="coerce")
            if s.notna().any():
                return s

    # Combine Date + Time if present.
    date_col = None
    time_col = None
    for c in cols:
        cl = c.lower()
        if date_col is None and re.search(r"\bdate\b", cl):
            date_col = c
        if time_col is None and re.search(r"\btime\b", cl):
            time_col = c

    if date_col and time_col:
        date_s = df[date_col].astype(str).str.strip()
        time_s = df[time_col].astype(str).str.strip()

        # Heuristic for day/month ordering.
        sample_dates = date_s.head(200)
        iso_like = sample_dates.str.match(r"^\d{4}-\d{2}-\d{2}$", na=False).mean() > 0.8
        textual_month = sample_dates.str.contains(r"[A-Za-z]{3}", regex=True, na=False).mean() > 0.2
        use_dayfirst = bool(textual_month and not iso_like)

        # First pass: combined parse with day-first for legacy formats like 1-Jun-2001 0:15:52.
        combo = date_s + " " + time_s
        try:
            s = pd.to_datetime(combo, errors="coerce", dayfirst=use_dayfirst, format="mixed")
        except TypeError:
            s = pd.to_datetime(combo, errors="coerce", dayfirst=use_dayfirst)

        # Second pass for numeric Excel-like time/date values.
        t_num = pd.to_numeric(time_s, errors="coerce")
        missing = s.isna()
        if missing.any():
            # Parse date column alone.
            try:
                d = pd.to_datetime(date_s, errors="coerce", dayfirst=use_dayfirst, format="mixed")
            except TypeError:
                d = pd.to_datetime(date_s, errors="coerce", dayfirst=use_dayfirst)

            # Excel fraction-of-day in [0,1]: combine with parsed date.
            frac_mask = missing & t_num.notna() & (t_num >= 0) & (t_num < 1) & d.notna()
            if frac_mask.any():
                s.loc[frac_mask] = d.loc[frac_mask] + pd.to_timedelta(
                    t_num.loc[frac_mask] * 24 * 3600, unit="s"
                )

            # Excel serial datetime (>1): convert directly.
            serial_mask = missing & t_num.notna() & (t_num >= 1) & (t_num < 100000)
            if serial_mask.any():
                s.loc[serial_mask] = pd.to_datetime(
                    t_num.loc[serial_mask], unit="D", origin="1899-12-30", errors="coerce"
                )

        if s.notna().any():
            return s

    if date_col:
        date_s = df[date_col].astype(str).str.strip()
        iso_like = date_s.head(200).str.match(r"^\d{4}-\d{2}-\d{2}$", na=False).mean() > 0.8
        textual_month = date_s.head(200).str.contains(r"[A-Za-z]{3}", regex=True, na=False).mean() > 0.2
        use_dayfirst = bool(textual_month and not iso_like)
        try:
            s = pd.to_datetime(date_s, errors="coerce", dayfirst=use_dayfirst, format="mixed")
        except TypeError:
            s = pd.to_datetime(date_s, errors="coerce", dayfirst=use_dayfirst)
        if s.notna().any():
            return s

    return pd.Series(pd.NaT, index=df.index)


def summarize_file(csv_path: Path, package_id: str) -> dict:
    try:
        df = pd.read_csv(csv_path, low_memory=False)
    except Exception as e:
        return {
            "package_id": package_id,
            "file": str(csv_path),
            "site_code": "UNKNOWN",
            "site_name": "UNKNOWN",
            "year": "",
            "rows": -1,
            "valid_time_rows": 0,
            "start": "",
            "end": "",
            "interval_min": np.nan,
            "expected_rows": np.nan,
            "completeness_pct": np.nan,
            "n_gaps_gt_1h": np.nan,
            "n_gaps_gt_6h": np.nan,
            "n_gaps_gt_24h": np.nan,
            "max_gap_hours": np.nan,
            "n_segments_gt_24h_rule": np.nan,
            "read_error": str(e),
        }

    dt = infer_datetime_series(df)
    dt = dt.dropna().sort_values().drop_duplicates()
    sample = df.head(50).copy()
    site_code = infer_site_code(csv_path.name, list(df.columns), sample)
    site_name = infer_site_name(site_code)

    out = {
        "package_id": package_id,
        "file": str(csv_path),
        "site_code": site_code,
        "site_name": site_name,
        "year": "",
        "rows": int(len(df)),
        "valid_time_rows": int(len(dt)),
        "start": "",
        "end": "",
        "interval_min": np.nan,
        "expected_rows": np.nan,
        "completeness_pct": np.nan,
        "n_gaps_gt_1h": np.nan,
        "n_gaps_gt_6h": np.nan,
        "n_gaps_gt_24h": np.nan,
        "max_gap_hours": np.nan,
        "n_segments_gt_24h_rule": np.nan,
        "read_error": "",
    }

    if len(dt) == 0:
        return out

    start = dt.iloc[0]
    end = dt.iloc[-1]
    out["start"] = start.isoformat()
    out["end"] = end.isoformat()
    out["year"] = str(start.year)

    if len(dt) >= 2:
        diffs_min = dt.diff().dropna().dt.total_seconds() / 60.0
        diffs_min = diffs_min[diffs_min > 0]
        if len(diffs_min) > 0:
            interval_min = float(pd.Series(diffs_min).mode().iloc[0])
            out["interval_min"] = interval_min
            span_min = (end - start).total_seconds() / 60.0
            expected_rows = int(np.floor(span_min / interval_min) + 1) if interval_min > 0 else np.nan
            out["expected_rows"] = expected_rows
            if expected_rows and expected_rows > 0:
                out["completeness_pct"] = round(100.0 * len(dt) / expected_rows, 2)

            gaps_h = (dt.diff().dropna().dt.total_seconds() / 3600.0).astype(float)
            out["n_gaps_gt_1h"] = int((gaps_h > 1.0).sum())
            out["n_gaps_gt_6h"] = int((gaps_h > 6.0).sum())
            out["n_gaps_gt_24h"] = int((gaps_h > 24.0).sum())
            out["max_gap_hours"] = float(gaps_h.max()) if len(gaps_h) else 0.0
            out["n_segments_gt_24h_rule"] = int((gaps_h > 24.0).sum() + 1) if len(gaps_h) else 1

    # Prefer year from filename if present.
    m = re.search(r"(19|20)\d{2}", csv_path.name)
    if m:
        out["year"] = m.group(0)

    return out


def main() -> int:
    p = argparse.ArgumentParser(description="Summarize sonde site/year coverage and gaps.")
    p.add_argument("--root", type=Path, default=Path("data/raw/pasta_sonde"))
    p.add_argument("--out-dir", type=Path, default=Path("repro/reports/sonde_coverage"))
    args = p.parse_args()

    root = args.root
    out_dir = args.out_dir
    out_dir.mkdir(parents=True, exist_ok=True)

    csv_files = sorted(root.rglob("*.csv"))
    rows = []
    for f in csv_files:
        pkg = "UNKNOWN"
        for parent in f.parents:
            if re.match(r"knb-lter-pie\.\d+\.\d+$", parent.name):
                pkg = parent.name
                break
        row = summarize_file(f, pkg)
        if row["site_code"] in EXCLUDE_SITE_CODES:
            continue
        rows.append(row)

    df = pd.DataFrame(rows).sort_values(["site_code", "year", "file"])
    detailed_path = out_dir / "sonde_file_coverage_detailed.csv"
    df.to_csv(detailed_path, index=False)

    # Site-year aggregate across files (handles occasional duplicate csv/xls exports).
    agg_cols = {
        "rows": "sum",
        "valid_time_rows": "sum",
        "start": "min",
        "end": "max",
        "completeness_pct": "mean",
        "n_gaps_gt_1h": "sum",
        "n_gaps_gt_6h": "sum",
        "n_gaps_gt_24h": "sum",
        "max_gap_hours": "max",
        "n_segments_gt_24h_rule": "sum",
    }
    site_year = (
        df.groupby(["site_code", "site_name", "year"], dropna=False)
        .agg(agg_cols)
        .reset_index()
        .sort_values(["site_code", "year"])
    )
    site_year_path = out_dir / "sonde_site_year_summary.csv"
    site_year.to_csv(site_year_path, index=False)

    site_summary = (
        site_year.groupby(["site_code", "site_name"], dropna=False)
        .agg(
            years_with_data=("year", "count"),
            first_year=("year", "min"),
            last_year=("year", "max"),
            first_date=("start", "min"),
            last_date=("end", "max"),
            min_completeness_pct=("completeness_pct", "min"),
            max_gap_hours_any_year=("max_gap_hours", "max"),
            total_gaps_gt_24h=("n_gaps_gt_24h", "sum"),
        )
        .reset_index()
        .sort_values(["site_code"])
    )
    site_summary_path = out_dir / "sonde_site_summary.csv"
    site_summary.to_csv(site_summary_path, index=False)

    missing_rows = []
    for _, r in site_summary.iterrows():
        years = sorted(site_year.loc[site_year["site_code"] == r["site_code"], "year"].astype(int).tolist())
        yset = set(years)
        all_years = list(range(min(years), max(years) + 1))
        missing = [y for y in all_years if y not in yset]
        missing_rows.append(
            {
                "site_code": r["site_code"],
                "site_name": r["site_name"],
                "first_year": min(years),
                "last_year": max(years),
                "missing_years_within_span": ";".join(map(str, missing)),
                "n_missing_years_within_span": len(missing),
            }
        )
    missing_df = pd.DataFrame(missing_rows).sort_values(["site_code"])
    missing_path = out_dir / "sonde_site_missing_years.csv"
    missing_df.to_csv(missing_path, index=False)

    # Site-level availability matrix.
    matrix = (
        site_year.assign(has_data=1)
        .pivot_table(index=["site_code", "site_name"], columns="year", values="has_data", fill_value=0)
        .reset_index()
    )
    matrix_path = out_dir / "sonde_site_year_matrix.csv"
    matrix.to_csv(matrix_path, index=False)

    # Package to file lookup.
    pkg_lookup = (
        df.groupby("package_id")
        .agg(
            files=("file", "count"),
            sites=("site_code", lambda x: ";".join(sorted(set(map(str, x))))),
            years=("year", lambda x: ";".join(sorted(set(map(str, x))))),
            start=("start", "min"),
            end=("end", "max"),
        )
        .reset_index()
        .sort_values("package_id")
    )
    pkg_lookup_path = out_dir / "sonde_package_lookup.csv"
    pkg_lookup.to_csv(pkg_lookup_path, index=False)

    print(f"Wrote {detailed_path}")
    print(f"Wrote {site_year_path}")
    print(f"Wrote {site_summary_path}")
    print(f"Wrote {missing_path}")
    print(f"Wrote {matrix_path}")
    print(f"Wrote {pkg_lookup_path}")
    print(f"CSV files summarized: {len(df)}")
    print(f"Excluded site codes: {', '.join(sorted(EXCLUDE_SITE_CODES))}")
    print(f"Unique sites: {df['site_code'].nunique()}")
    print(f"Years: {df['year'].replace('', np.nan).dropna().nunique()}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
