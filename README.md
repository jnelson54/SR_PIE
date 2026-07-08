# SR_PIE

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

See `repro/PIPELINE_PLAN.md` for the end-to-end submission-ready pipeline plan.

## R Sonde Analysis Pipeline

The full reproducible sonde anomaly workflow for the manuscript is in:

- `analysis/run_all.R`
- `analysis/README.md`

It uses original files under `data/raw/pasta_sonde`, applies the study inclusion rules
(`IBYC+IB`, `MB+MR`, exclude `RR`/`RO`/`RT1A`), and builds June-July and August-September
seasonal raw and anomaly outputs plus publication-ready figures.

```powershell
"C:\Program Files\R\R-4.5.2\bin\Rscript.exe" analysis\run_all.R
```

## Student Onboarding

For student collaborators using VS Code, use:

- `docs/github_vscode_onboarding.md`
