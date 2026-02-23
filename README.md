# NCC_PIE

Reproducible analysis scaffold for the Plum Island Estuary manuscript workflow.

## Current status

- Manuscript-core source packages are listed in:
  - `repro/manifests/current_manuscript_core_packages.csv`
- Full estuarine moored sonde package manifest is in:
  - `repro/manifests/full_estuarine_moored_sonde_packages.csv`
- Download/audit tooling is in:
  - `repro/scripts/download_pasta_packages.py`
  - `repro/scripts/audit_manifest_local.py`

## Quick start

```powershell
python repro/scripts/audit_manifest_local.py `
  --manifest repro/manifests/current_manuscript_core_packages.csv `
  --local-dir .
```

```powershell
python repro/scripts/download_pasta_packages.py `
  --manifest repro/manifests/current_manuscript_core_packages.csv `
  --out-dir data/raw/pasta `
  --extract
```

See `repro/PIPELINE_PLAN.md` for the end-to-end Nature Communications-grade pipeline plan.

