# Plankton Figure Submodule

This folder contains the food-web mixing outputs and scripts used for Figure 3.

Primary script:
- `fig_plankton_trends_nyt_with_fiddler.R`

Conventions:
- Marine source is labeled consistently as `marine plankton (POM)`.
- Trend significance is classified with Benjamini-Hochberg FDR-adjusted q-values.

Run from this folder:
- `Rscript fig_plankton_trends_nyt_with_fiddler.R --mix aggregated_contributions.csv`

Optional MixSIAR rerun:
- `Rscript mixsiar_ncc_fids.R <path_to_mixsiar_input_folder>`
