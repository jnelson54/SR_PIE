# SR_PIE — Data and analysis code for the Scientific Reports manuscript

**Warming, salinification, and food-web rewiring precede poleward expansion of a marsh ecosystem engineer**
Nelson et al., Plum Island Ecosystems (PIE) LTER.

This repository contains the analysis code, source-data tables, and data-provenance
manifests needed to reproduce the results and figures in the manuscript. Primary
long-term data are archived at the Environmental Data Initiative (EDI); this repository
documents exactly which EDI packages were used and provides the derived, analysis-ready
outputs.

## Contents

- `analysis/` — R pipeline for the estuarine sonde hydrography analysis.
  - `run_all.R` runs `01_prepare_sonde_seasonal_data.R` -> `02_fit_trends_and_anomalies.R` -> `03_make_publication_figures.R`.
  - Inclusion rules: IBYC+IB (lower estuary), MB+MR (upper estuary); RR/RO/RT1A excluded.
  - Outputs written to `analysis/output/sonde/` and figures to `analysis/figures/`.
- `repro/` — reproducibility tooling and EDI data-package provenance.
  - `manifests/current_manuscript_core_packages.csv` — the core EDI packages used (with DOIs).
  - `manifests/full_estuarine_moored_sonde_packages.csv` — full per-year/per-site sonde package list (2000–2025).
  - `scripts/download_pasta_packages.py` — downloads the listed EDI/PASTA packages.
  - `scripts/audit_manifest_local.py` — audits local copies against a manifest.
  - `reports/sonde_coverage/` — station-by-year coverage summaries.
- `submission_20260421/` — figure source-data tables and rendered figures underlying Figs 1–4 and Supplementary Fig. 1.

## Key data sources (EDI)

- Stable-isotope food-web data: `knb-lter-pie.13.11` (doi:10.6073/pasta/78895624039aaa0e75c6d71970fd7fb4).
- Estuarine moored sonde hydrography (temperature/conductivity): per-year/per-site packages listed in `repro/manifests/full_estuarine_moored_sonde_packages.csv`.
- Parker River discharge: USGS NWIS station 01101000 (https://waterdata.usgs.gov/monitoring-location/01101000/).

## Reproduce

```powershell
python repro/scripts/download_pasta_packages.py `
  --manifest repro/manifests/current_manuscript_core_packages.csv `
  --out-dir data/raw/pasta --extract

Rscript analysis/run_all.R
```

## Citation

See `CITATION.cff`. Please cite the associated article and this archive (Zenodo DOI).
