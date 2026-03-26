# Comment Alignment Summary

This note records the main manuscript and figure issues raised in the NCC comment files and how they were addressed in the synchronized submission package.

Primary comment themes addressed:

- Clarify that the mud fiddler crab signal reflects support from a marine plankton or POM pathway rather than direct adult filtration of phytoplankton.
  - Addressed in the synchronized manuscript text in `manuscript/NCC_Fiddler_Niche_submission_ready_2026-03-24.txt`.
  - Addressed in the Figure 3 plotting labels and source-data summary.

- Use clearer Figure 3 wording for plankton contributions.
  - Addressed in `analysis/plankton/fig_plankton_trends_nyt_with_fiddler.R` by updating panel titles and the fiddler annotation.

- Ensure the fiddler values in the manuscript and Figure 3 package reflect the latest rerun.
  - Addressed by aligning `analysis/plankton/fiddler_crab_contributions_filtered.csv`, `results/plankton/fiddler_mixsiar_summary.csv`, `source_data/Figure3_fiddler_mixsiar_summary.csv`, and the synchronized manuscript text to the canonical 20-crab rerun.

- Clarify the freshwater discharge interpretation.
  - Addressed in the synchronized manuscript text by treating discharge as contextual and not as a sole causal explanation for salinity change.

- Ensure supplementary burrow trend information is represented in the package.
  - Present in `results/burrow/Supplementary_Table_1_burrow_trends.csv` and noted in `source_data/README.md`.

- Ensure the Parker River supplementary figure exists as a single journal-facing figure.
  - Addressed in `analysis/discharge/analysis_parker_discharge_usgs.R` by exporting `figures/Supplementary_Figure1_parker_discharge.pdf` and `figures/Supplementary_Figure1_parker_discharge.png`.

- Exclude the exploratory study-area map workflow from the journal-facing and GitHub submission package.
  - Addressed by keeping the map workflow out of the active manuscript code-availability text and by excluding `analysis/study_area/` and `figures/fig_study_area_map.*` from git upload via `.gitignore`.

Administrative notes:

- The formatted manuscript `.docx` in `manuscript/` was not directly edited in this workspace.
- The synchronized plain-text manuscript source should be treated as the authoritative editable text until the layout manuscript is refreshed from it.
- Archived non-canonical analytical material is retained only for provenance and should not be used for reported manuscript values.
