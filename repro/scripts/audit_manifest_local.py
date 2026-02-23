#!/usr/bin/env python3
"""Audit whether manifest package IDs are present locally."""

from __future__ import annotations

import argparse
import csv
from pathlib import Path
from typing import Dict, List


def parse_package_id(package_id: str) -> tuple[str, str]:
    parts = package_id.strip().split(".")
    if len(parts) != 3:
        raise ValueError(f"Invalid package_id: {package_id}")
    return parts[1], parts[2]


def read_manifest(path: Path) -> List[Dict[str, str]]:
    with path.open("r", encoding="utf-8", newline="") as f:
        reader = csv.DictReader(f)
        if "package_id" not in reader.fieldnames:
            raise ValueError(f"{path} is missing required column 'package_id'")
        return list(reader)


def find_presence(package_id: str, local_dir: Path) -> dict[str, str]:
    ident, rev = parse_package_id(package_id)
    package_xml = f"knb-lter-pie.{ident}.{rev}.xml"
    package_zip = f"{package_id}.zip"

    xml_hits = list(local_dir.rglob(package_xml))
    zip_hits = list(local_dir.rglob(package_zip))
    return {
        "package_id": package_id,
        "present_xml": str(bool(xml_hits)).lower(),
        "present_zip": str(bool(zip_hits)).lower(),
        "xml_path": str(xml_hits[0]) if xml_hits else "",
        "zip_path": str(zip_hits[0]) if zip_hits else "",
        "present_any": str(bool(xml_hits or zip_hits)).lower(),
    }


def main() -> int:
    p = argparse.ArgumentParser(description="Audit local presence of PASTA package IDs.")
    p.add_argument("--manifest", required=True, type=Path, help="CSV containing package_id column.")
    p.add_argument("--local-dir", type=Path, default=Path("."), help="Directory to search recursively.")
    p.add_argument(
        "--out",
        type=Path,
        default=Path("repro/audit_manifest_local.tsv"),
        help="TSV report output path.",
    )
    args = p.parse_args()

    rows = read_manifest(args.manifest)
    results = []
    for row in rows:
        package_id = row["package_id"].strip()
        results.append(find_presence(package_id, args.local_dir))

    args.out.parent.mkdir(parents=True, exist_ok=True)
    with args.out.open("w", encoding="utf-8", newline="") as f:
        writer = csv.DictWriter(
            f,
            fieldnames=["package_id", "present_any", "present_xml", "present_zip", "xml_path", "zip_path"],
            delimiter="\t",
        )
        writer.writeheader()
        writer.writerows(results)

    present = sum(r["present_any"] == "true" for r in results)
    print(f"Packages in manifest: {len(results)}")
    print(f"Present locally: {present}")
    print(f"Missing locally: {len(results) - present}")
    print(f"Report: {args.out}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
