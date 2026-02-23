#!/usr/bin/env python3
"""Download PASTA EML packages listed in a manifest CSV.

Expected manifest column: package_id (example: knb-lter-pie.4.12)
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import os
import time
import urllib.error
import urllib.request
import zipfile
from pathlib import Path
from typing import Iterable, List, Tuple


def parse_package_id(package_id: str) -> Tuple[str, str, str]:
    parts = package_id.strip().split(".")
    if len(parts) != 3:
        raise ValueError(f"Invalid package_id: {package_id}")
    scope = parts[0]
    identifier = parts[1]
    revision = parts[2]
    if not identifier.isdigit() or not revision.isdigit():
        raise ValueError(f"Invalid package_id: {package_id}")
    return scope, identifier, revision


def read_manifest(path: Path) -> List[str]:
    with path.open("r", encoding="utf-8", newline="") as f:
        reader = csv.DictReader(f)
        if "package_id" not in reader.fieldnames:
            raise ValueError(f"{path} is missing required column 'package_id'")
        return [row["package_id"].strip() for row in reader if row.get("package_id", "").strip()]


def sha256_file(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as f:
        for chunk in iter(lambda: f.read(1024 * 1024), b""):
            h.update(chunk)
    return h.hexdigest()


def download_url(url: str, dest: Path, retries: int = 6) -> None:
    dest.parent.mkdir(parents=True, exist_ok=True)
    last_err = None
    for attempt in range(1, retries + 1):
        try:
            req = urllib.request.Request(url, headers={"User-Agent": "repro-pipeline/1.0"})
            with urllib.request.urlopen(req, timeout=120) as r:
                if r.status != 200:
                    raise urllib.error.HTTPError(url, r.status, f"HTTP {r.status}", r.headers, None)
                with dest.open("wb") as out:
                    while True:
                        chunk = r.read(1024 * 1024)
                        if not chunk:
                            break
                        out.write(chunk)
            return
        except Exception as e:  # noqa: BLE001
            last_err = e
            if attempt == retries:
                break
            sleep_s = min(2 ** attempt, 20)
            time.sleep(sleep_s)
    raise RuntimeError(f"Failed download: {url} ({last_err})")


def maybe_extract(zip_path: Path, extract_dir: Path) -> None:
    extract_dir.mkdir(parents=True, exist_ok=True)
    with zipfile.ZipFile(zip_path, "r") as zf:
        zf.extractall(extract_dir)


def run(manifest: Path, out_dir: Path, extract: bool, force: bool, sleep_s: float) -> int:
    package_ids = read_manifest(manifest)
    if not package_ids:
        print("No package_id rows found in manifest.")
        return 0

    print(f"Manifest packages: {len(package_ids)}")
    failures = 0
    for idx, package_id in enumerate(package_ids, start=1):
        try:
            scope, ident, rev = parse_package_id(package_id)
            url = f"https://pasta.lternet.edu/package/download/eml/{scope}/{ident}/{rev}"
            zip_path = out_dir / f"{package_id}.zip"
            extract_path = out_dir / package_id

            if zip_path.exists() and not force:
                print(f"[{idx}/{len(package_ids)}] SKIP existing {zip_path.name}")
            else:
                print(f"[{idx}/{len(package_ids)}] GET  {package_id}")
                download_url(url, zip_path)
                print(f"  saved {zip_path} sha256={sha256_file(zip_path)}")

            if extract:
                if extract_path.exists() and force:
                    for child in extract_path.glob("*"):
                        if child.is_file():
                            child.unlink()
                maybe_extract(zip_path, extract_path)
                print(f"  extracted -> {extract_path}")

            if sleep_s > 0:
                time.sleep(sleep_s)
        except Exception as e:  # noqa: BLE001
            failures += 1
            print(f"  ERROR {package_id}: {e}")

    print(f"Done. failures={failures}")
    return 1 if failures else 0


def main() -> int:
    p = argparse.ArgumentParser(description="Download PASTA packages listed in a manifest CSV.")
    p.add_argument("--manifest", required=True, type=Path, help="CSV containing package_id column.")
    p.add_argument(
        "--out-dir",
        type=Path,
        default=Path("data/raw/pasta"),
        help="Directory for downloaded package zip files.",
    )
    p.add_argument("--extract", action="store_true", help="Extract each zip to out-dir/<package_id>/")
    p.add_argument("--force", action="store_true", help="Re-download existing files.")
    p.add_argument("--sleep", type=float, default=0.5, help="Delay between package downloads.")
    args = p.parse_args()
    return run(args.manifest, args.out_dir, args.extract, args.force, args.sleep)


if __name__ == "__main__":
    raise SystemExit(main())
