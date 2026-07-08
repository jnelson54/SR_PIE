# PIE Analysis Repository (Scientific Reports Submission)

This repository package contains the analysis code, input data, derived tables, and figure outputs used for the Global Change Biology manuscript submission:

- `Boundary forcing and food-web rewiring permit range expansion`
- Manuscript document files are excluded from this repository.

## Repository Layout

- `analysis/hydrography/`
  - Sonde hydrography analysis for Fig. 1 (`analysis_pie_temp_sal_sonde.R`)
  - Inputs: `pie_temp_sal_sonde.csv`, `High Tide Values.csv`
- `analysis/burrow/`
  - Burrow-density analysis script and source data
- `analysis/plankton/`
  - MixSIAR inputs/outputs and plankton-trend figure scripts
  - Includes `fig_plankton_trends_nyt_with_fiddler.R` (BH-FDR q-value trend labeling)
- `analysis/common/theme_nyt.R`
  - Shared plotting theme used across scripts
- `results/`
  - Derived summary tables used in manuscript text
- `figures/`
  - Figure files used for manuscript assembly
- `environment/`
  - Dependency list

## Reproduction Order

Run commands from repository root using the subfolder working directory shown below.

1. Hydrography trends (Fig. 1 + summary tables)
   - `cd analysis/hydrography`
   - `Rscript analysis_pie_temp_sal_sonde.R`
2. Burrow analysis (Fig. 2 support)
   - `cd analysis/burrow`
   - `Rscript burrow_density_analysis.R`
3. Plankton trends figure (Fig. 3)
   - `cd analysis/plankton`
   - `Rscript fig_plankton_trends_nyt_with_fiddler.R --mix aggregated_contributions.csv`
4. Optional MixSIAR rerun
   - `cd analysis/plankton`
   - `Rscript mixsiar_gcb_fids.R <path_to_mixsiar_input_folder>`

## Statistical Reporting Convention

This submission package uses Benjamini-Hochberg false discovery rate adjustment and reports inferential results as **q-values**.

## Notes

- MixSIAR reruns require local JAGS/MixSIAR setup.
- This package includes both source data and precomputed outputs used for manuscript figures/tables.
