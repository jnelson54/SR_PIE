#!/usr/bin/env python3
"""Build combined-site sonde accounting (monthly coverage + variable availability).

Rules applied:
- LOWER_MAIN = IBYC + IB
- MIDDLE_MAIN = MB + MR
- Exclude RR, RO, and RT1A
"""

from __future__ import annotations

import argparse
import re
from pathlib import Path

import numpy as np
import pandas as pd


EXCLUDE_SITE_CODES = {"RR", "RO", "RT1A"}
COMBINED_SITE_MAP = {
    "IBYC": "LOWER_MAIN",
    "IB": "LOWER_MAIN",
    "MB": "MIDDLE_MAIN",
    "MR": "MIDDLE_MAIN",
}


def infer_site_code(file_name: str, sample: pd.DataFrame) -> str:
    fn = file_name.upper()
    m = re.search(r"_YSI_([A-Z0-9]+)\.CSV$", fn)
    if m:
        return m.group(1)
    if "MON-PR-MBYSI" in fn:
        return "MB"
    if "MON-PR-RRYSI" in fn:
        return "RR"
    if "MON-SO-IBYCYSI" in fn:
        return "IBYC"
    if "MON-PR-RT1AYSI" in fn:
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


def infer_datetime_series(df: pd.DataFrame) -> pd.Series:
    cols = list(df.columns)
    lower = {c.lower(): c for c in cols}
    for key in ["timestamp", "datetime", "date_time", "sampledatetime", "dateandtime"]:
        if key in lower:
            s = pd.to_datetime(df[lower[key]], errors="coerce")
            if s.notna().any():
                return s

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
        sample_dates = date_s.head(200)
        iso_like = sample_dates.str.match(r"^\d{4}-\d{2}-\d{2}$", na=False).mean() > 0.8
        textual_month = sample_dates.str.contains(r"[A-Za-z]{3}", regex=True, na=False).mean() > 0.2
        use_dayfirst = bool(textual_month and not iso_like)

        combo = date_s + " " + time_s
        try:
            s = pd.to_datetime(combo, errors="coerce", dayfirst=use_dayfirst, format="mixed")
        except TypeError:
            s = pd.to_datetime(combo, errors="coerce", dayfirst=use_dayfirst)

        t_num = pd.to_numeric(time_s, errors="coerce")
        missing = s.isna()
        if missing.any():
            try:
                d = pd.to_datetime(date_s, errors="coerce", dayfirst=use_dayfirst, format="mixed")
            except TypeError:
                d = pd.to_datetime(date_s, errors="coerce", dayfirst=use_dayfirst)

            frac_mask = missing & t_num.notna() & (t_num >= 0) & (t_num < 1) & d.notna()
            if frac_mask.any():
                s.loc[frac_mask] = d.loc[frac_mask] + pd.to_timedelta(
                    t_num.loc[frac_mask] * 24 * 3600, unit="s"
                )

            serial_mask = missing & t_num.notna() & (t_num >= 1) & (t_num < 100000)
            if serial_mask.any():
                s.loc[serial_mask] = pd.to_datetime(
                    t_num.loc[serial_mask], unit="D", origin="1899-12-30", errors="coerce"
                )
        if s.notna().any():
            return s

    if date_col:
        date_s = df[date_col].astype(str).str.strip()
        sample_dates = date_s.head(200)
        iso_like = sample_dates.str.match(r"^\d{4}-\d{2}-\d{2}$", na=False).mean() > 0.8
        textual_month = sample_dates.str.contains(r"[A-Za-z]{3}", regex=True, na=False).mean() > 0.2
        use_dayfirst = bool(textual_month and not iso_like)
        try:
            s = pd.to_datetime(date_s, errors="coerce", dayfirst=use_dayfirst, format="mixed")
        except TypeError:
            s = pd.to_datetime(date_s, errors="coerce", dayfirst=use_dayfirst)
        if s.notna().any():
            return s

    return pd.Series(pd.NaT, index=df.index)


def canonical_variables(columns: list[str]) -> dict[str, list[str]]:
    # Keep original names but map to canonical buckets for comparability.
    keep = []
    for c in columns:
        cl = c.lower().strip()
        if cl in {"date", "time", "timestamp", "datetime", "station", "comments"}:
            continue
        if cl.startswith("flag_"):
            continue
        keep.append(c)

    buckets = {
        "temperature": [],
        "salinity": [],
        "conductivity": [],
        "dissolved_oxygen": [],
        "depth": [],
        "ph": [],
        "turbidity": [],
        "chlorophyll_algae": [],
        "fdom_fluorescence": [],
        "other": [],
    }
    for c in keep:
        cl = c.lower()
        if "temp" in cl:
            buckets["temperature"].append(c)
        elif "salinity" in cl:
            buckets["salinity"].append(c)
        elif "chlorophyll" in cl or "algae" in cl or "phyco" in cl:
            buckets["chlorophyll_algae"].append(c)
        elif "fdom" in cl or "fluores" in cl:
            buckets["fdom_fluorescence"].append(c)
        elif "spcond" in cl or re.fullmatch(r"cond", cl) or "conduct" in cl:
            buckets["conductivity"].append(c)
        elif cl.startswith("do") or "odo" in cl or "dissolved oxygen" in cl:
            buckets["dissolved_oxygen"].append(c)
        elif "depth" in cl:
            buckets["depth"].append(c)
        elif re.fullmatch(r"ph", cl) or "ph" in cl:
            buckets["ph"].append(c)
        elif "turbid" in cl:
            buckets["turbidity"].append(c)
        else:
            buckets["other"].append(c)
    return buckets


def main() -> int:
    p = argparse.ArgumentParser(description="Monthly accounting for combined sonde sites.")
    p.add_argument("--root", type=Path, default=Path("data/raw/pasta_sonde"))
    p.add_argument("--out-dir", type=Path, default=Path("repro/reports/sonde_coverage"))
    args = p.parse_args()

    out_dir = args.out_dir
    out_dir.mkdir(parents=True, exist_ok=True)

    monthly_rows = []
    file_rows = []
    var_year_rows = []
    canonical_monthly_rows = []

    for csv_path in sorted(args.root.rglob("*.csv")):
        pkg = next((p.name for p in csv_path.parents if re.match(r"knb-lter-pie\.\d+\.\d+$", p.name)), "UNKNOWN")
        try:
            df = pd.read_csv(csv_path, low_memory=False)
        except Exception:
            continue
        site_code = infer_site_code(csv_path.name, df.head(50))
        if site_code in EXCLUDE_SITE_CODES:
            continue
        combined = COMBINED_SITE_MAP.get(site_code)
        if not combined:
            continue

        dt = infer_datetime_series(df)
        valid = dt.notna()
        if valid.sum() == 0:
            continue

        df2 = df.loc[valid].copy()
        df2["dt"] = dt.loc[valid]
        df2["year"] = df2["dt"].dt.year.astype(int)
        df2["month"] = df2["dt"].dt.month.astype(int)

        raw_cols = [c for c in df.columns if c not in {"dt", "year", "month"}]
        buckets = canonical_variables(raw_cols)
        raw_vars = sorted(set(sum(buckets.values(), [])))
        canonical_present = sorted([k for k, v in buckets.items() if k != "other" and len(v) > 0])

        file_rows.append(
            {
                "package_id": pkg,
                "file": str(csv_path),
                "site_code": site_code,
                "combined_site": combined,
                "start": df2["dt"].min().isoformat(),
                "end": df2["dt"].max().isoformat(),
                "rows": len(df2),
                "raw_variables": ";".join(raw_vars),
                "canonical_variables": ";".join(canonical_present),
            }
        )

        # Monthly windows.
        m = (
            df2.groupby(["year", "month"])
            .agg(start=("dt", "min"), end=("dt", "max"), rows=("dt", "count"), days=("dt", lambda x: x.dt.date.nunique()))
            .reset_index()
        )
        for _, r in m.iterrows():
            monthly_rows.append(
                {
                    "combined_site": combined,
                    "source_site_code": site_code,
                    "package_id": pkg,
                    "file": str(csv_path),
                    "year": int(r["year"]),
                    "month": int(r["month"]),
                    "start": r["start"].isoformat(),
                    "end": r["end"].isoformat(),
                    "rows": int(r["rows"]),
                    "days_with_data": int(r["days"]),
                }
            )

        # Variables by year.
        for y, g in df2.groupby("year"):
            # non-null raw vars in this year
            available_raw = []
            for c in raw_vars:
                if c in g.columns and g[c].notna().any():
                    available_raw.append(c)
            available_canonical = []
            for cname, cols in buckets.items():
                if cname == "other":
                    continue
                if any((col in g.columns and g[col].notna().any()) for col in cols):
                    available_canonical.append(cname)
            var_year_rows.append(
                {
                    "combined_site": combined,
                    "source_site_code": site_code,
                    "year": int(y),
                    "file": str(csv_path),
                    "raw_variables_available": ";".join(sorted(available_raw)),
                    "canonical_variables_available": ";".join(sorted(available_canonical)),
                }
            )

        # Canonical variable availability by month.
        for (y, mo), g in df2.groupby(["year", "month"]):
            row = {
                "combined_site": combined,
                "source_site_code": site_code,
                "year": int(y),
                "month": int(mo),
            }
            for cname, cols in buckets.items():
                if cname == "other":
                    continue
                row[cname] = int(any((col in g.columns and g[col].notna().any()) for col in cols))
            canonical_monthly_rows.append(row)

    monthly_df = pd.DataFrame(monthly_rows)
    file_df = pd.DataFrame(file_rows)
    var_year_df = pd.DataFrame(var_year_rows)
    canonical_monthly_df = pd.DataFrame(canonical_monthly_rows)

    # Collapse to combined-site monthly windows across all source files.
    combined_monthly = (
        monthly_df.groupby(["combined_site", "year", "month"], dropna=False)
        .agg(
            start=("start", "min"),
            end=("end", "max"),
            rows=("rows", "sum"),
            days_with_data=("days_with_data", "sum"),
            source_sites=("source_site_code", lambda x: ";".join(sorted(set(x)))),
            packages=("package_id", lambda x: ";".join(sorted(set(x)))),
        )
        .reset_index()
        .sort_values(["combined_site", "year", "month"])
    )

    combined_year = (
        combined_monthly.groupby(["combined_site", "year"], dropna=False)
        .agg(
            start=("start", "min"),
            end=("end", "max"),
            months_with_data=("month", lambda x: ";".join(str(v) for v in sorted(set(map(int, x))))),
            n_months=("month", lambda x: len(set(map(int, x)))),
            rows=("rows", "sum"),
            source_sites=("source_sites", lambda x: ";".join(sorted(set(";".join(x).split(";"))))),
        )
        .reset_index()
        .sort_values(["combined_site", "year"])
    )

    # Canonical monthly availability collapsed to combined site.
    if len(canonical_monthly_df) > 0:
        canonical_cols = [c for c in canonical_monthly_df.columns if c not in {"combined_site", "source_site_code", "year", "month"}]
        agg = {c: "max" for c in canonical_cols}
        canonical_monthly_combined = (
            canonical_monthly_df.groupby(["combined_site", "year", "month"], dropna=False)
            .agg(agg)
            .reset_index()
            .sort_values(["combined_site", "year", "month"])
        )
    else:
        canonical_monthly_combined = pd.DataFrame()

    # Canonical + raw variables by year (combined site)
    if len(var_year_df) > 0:
        def merge_semi(vals: pd.Series) -> str:
            out = set()
            for v in vals:
                if isinstance(v, str) and v.strip():
                    out.update([x for x in v.split(";") if x])
            return ";".join(sorted(out))

        combined_var_year = (
            var_year_df.groupby(["combined_site", "year"], dropna=False)
            .agg(
                canonical_variables_available=("canonical_variables_available", merge_semi),
                raw_variables_available=("raw_variables_available", merge_semi),
            )
            .reset_index()
            .sort_values(["combined_site", "year"])
        )
    else:
        combined_var_year = pd.DataFrame()

    combined_monthly.to_csv(out_dir / "combined_site_monthly_timeframes.csv", index=False)
    combined_year.to_csv(out_dir / "combined_site_year_timeframes.csv", index=False)
    combined_var_year.to_csv(out_dir / "combined_site_year_variables.csv", index=False)
    canonical_monthly_combined.to_csv(out_dir / "combined_site_monthly_canonical_variables.csv", index=False)
    file_df.to_csv(out_dir / "combined_site_file_index.csv", index=False)

    # Human-readable quick summary to terminal.
    print("Combined sites included: LOWER_MAIN (IBYC+IB), MIDDLE_MAIN (MB+MR)")
    print("Excluded sites: RR, RO, RT1A")
    print(f"Files included: {len(file_df)}")
    print(f"Wrote {out_dir / 'combined_site_monthly_timeframes.csv'}")
    print(f"Wrote {out_dir / 'combined_site_year_timeframes.csv'}")
    print(f"Wrote {out_dir / 'combined_site_year_variables.csv'}")
    print(f"Wrote {out_dir / 'combined_site_monthly_canonical_variables.csv'}")
    print(f"Wrote {out_dir / 'combined_site_file_index.csv'}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
