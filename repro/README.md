# PIE NCC Reproducibility Scaffold

This folder contains a starter framework to make the manuscript analysis reproducible from original PIE/PASTA sources.

## What is here

- `manifests/current_manuscript_core_packages.csv`
  - Core package IDs needed for the current manuscript components (hydro anomaly, food web, fiddler density, recent sonde context).
- `manifests/full_estuarine_moored_sonde_packages.csv`
  - Full estuarine 15-minute moored sonde/water-quality suite discovered via PASTA metadata.
- `scripts/download_pasta_packages.py`
  - Downloads package ZIPs directly from PASTA using `package_id`.
- `scripts/audit_manifest_local.py`
  - Audits whether manifest package IDs are present in a local directory tree.

## Quick start

1. Audit local completeness for the manuscript core:

```bash
python repro/scripts/audit_manifest_local.py ^
  --manifest repro/manifests/current_manuscript_core_packages.csv ^
  --local-dir .
```

2. Download missing core packages (and extract):

```bash
python repro/scripts/download_pasta_packages.py ^
  --manifest repro/manifests/current_manuscript_core_packages.csv ^
  --out-dir data/raw/pasta ^
  --extract
```

3. Audit full estuarine moored sonde coverage:

```bash
python repro/scripts/audit_manifest_local.py ^
  --manifest repro/manifests/full_estuarine_moored_sonde_packages.csv ^
  --local-dir .
```

## Notes

- PASTA endpoint used by downloader:
  - `https://pasta.lternet.edu/package/download/eml/{scope}/{identifier}/{revision}`
- Keep raw downloads immutable under `data/raw/`; perform cleaning/harmonization in separate derived folders.
