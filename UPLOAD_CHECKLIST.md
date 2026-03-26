# Upload Checklist

Prepared package components for submission:

- `manuscript/NCC Fiddler_Niche_2026-03-05.docx`
- `manuscript/NCC_Fiddler_Niche_submission_ready_2026-03-24.txt`
- `README.md`
- `CODE_AVAILABILITY.md`
- `DATA_AVAILABILITY.md`
- `analysis/`
- `results/`
- `figures/`
- `source_data/`

Nature Communications submission workflow items that should be checked before upload:

1. Main manuscript file matches the regenerated outputs.
2. Figures and supplementary figures are up to date, including `figures/fig_study_area_map.pdf`, `figures/fig_study_area_map.png`, `figures/Supplementary_Figure1_parker_discharge.pdf`, and `figures/Supplementary_Figure1_parker_discharge.png`.
3. Numerical source data for each figure panel are present in `source_data/`.
4. Code and data availability statements in the manuscript match this package.
5. Any required reporting summary file is completed in the journal submission portal.

DataONE pre-deposit checks:

6. The synchronized text manuscript and the layout manuscript agree on all reported numerical values.
7. The active analysis paths and archived non-canonical paths are clearly distinguished in repository documentation.
8. Environment requirements in `environment/R_packages_minimum.txt` are included with the deposit metadata.
9. Package manifest and provenance notes are included with the deposited files.

GitHub upload preparation:

10. Initialize git in the repository root if needed and set `origin` to `https://github.com/jnelson54/NCC_PIE.git`.
11. Confirm `.gitignore` excludes only environment and transient backup files before `git add .`.

Zenodo archive steps:

12. Create a tagged GitHub release for the exact manuscript version you are depositing.
13. In Zenodo, enable the GitHub repository under GitHub integration and archive that tagged release to mint the DOI.
14. Add the Zenodo DOI back to the manuscript/repository metadata once minted.
