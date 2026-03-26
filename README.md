# NCC PIE Analysis Repository (Submission Package)

This repository is the consolidated submission package for the NCC manuscript and DataONE upload preparation.

- Layout manuscript: `manuscript/NCC Fiddler_Niche_2026-03-05.docx`
- Synchronized editable text: `manuscript/NCC_Fiddler_Niche_submission_ready_2026-03-24.txt`

The package includes five analysis components that map directly to the manuscript methods:

- `analysis/study_area/`
  - Plum Island Estuary study-area map with upper, middle, and lower sampling regions

- `analysis/hydrography/`
  - Sonde hydrography trends for estuary warming and conductivity change
- `analysis/discharge/`
  - Parker River discharge anomalies and pre/post-2014 tests
- `analysis/burrow/`
  - Mud fiddler burrow density and transect occupancy trends
- `analysis/plankton/`
  - Native consumer marine-plankton trend figure plus mud fiddler MixSIAR reference summary

Shared plotting helpers are in `analysis/common/theme_nyt.R`.

Derived outputs are written to:

- `results/`
- `figures/`
- `source_data/` once the source-data export step is run

Active provenance notes:

- The active fiddler crab analysis lives in `analysis/plankton/PIE_Mixing_Model/fiddler_crabs/`.
- Archived non-canonical or superseded material is retained under `archive_noncanonical_20260324/` and should not be used for manuscript values.
- The current authoritative fiddler rerun is the 20-crab two-isotope MixSIAR model using samples from `2023` and `2025`.

## Reproduction Order

Run from repository root with `Rscript` available on `PATH` or use the full Windows path to `Rscript.exe`.

1. `Rscript analysis/study_area/make_study_area_map.R`
2. `Rscript analysis/hydrography/analysis_pie_temp_sal_sonde.R`
3. `Rscript analysis/discharge/analysis_parker_discharge_usgs.R`
4. `Rscript analysis/burrow/burrow_density_analysis.R`
5. `Rscript analysis/plankton/fig_plankton_trends_nyt_with_fiddler.R --mix analysis/plankton/aggregated_contributions.csv`

Or run the wrapper:

- `Rscript run_all.R`

## Submission Contents

This bundle is organized to support both journal submission and DataONE deposit preparation:

- main manuscript file and synchronized text source
- analysis code with a code availability statement
- input and derived data with a data availability statement
- numerical source data underlying plotted figures
- provenance and package-level metadata describing active versus archived analysis products

If regenerated outputs diverge from the manuscript text, the synchronized text source should be updated first and the layout manuscript should then be refreshed from that text.
