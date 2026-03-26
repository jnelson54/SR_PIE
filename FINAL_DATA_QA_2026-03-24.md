# Final Data QA

Date: 2026-03-24
Package: `NatureComms_Analysis_Repository_2026-03-08_v2`

## Overall status

Final package QA passed for submission packaging and DataONE deposit preparation.

## Checks completed

- Generated an undated submission manuscript with embedded figures:
  - `manuscript/NCC_Fiddler_Niche_submission_ready.docx`
- Confirmed the generated Word document contains embedded media assets:
  - `word/media/image1.png`
  - `word/media/image2.png`
  - `word/media/image3.png`
  - `word/media/image4.png`
- Confirmed the synchronized editable manuscript source is present:
  - `manuscript/NCC_Fiddler_Niche_submission_ready_2026-03-24.txt`
- Confirmed package environment metadata is present:
  - `environment/R_packages_minimum.txt`
  - `environment/session_info_2026-03-24.txt`
- Re-ran the packaged analysis wrapper successfully from the repository root:
  - `Rscript run_all.R`
- Verified key Figure 3 summary outputs are aligned to the canonical 20-crab rerun:
  - POM `0.7336946749534305`
  - Spartina `0.25919771078931053`
  - benthic microalgae `0.007107614257258914`
- Cleared the remaining editor-detected issue in the hydrography script.

## Notes from QA

- The packaged analyses complete successfully from one command after path-resolution fixes in the hydrography, discharge, and burrow scripts.
- The burrow analysis emits a parsing warning from the CSV import, but the script completes and writes outputs.
- The plankton plotting script emits multiple warnings during execution, but it completes and writes outputs.
- The active submission package now contains both the formatted legacy manuscript and the updated undated submission-ready manuscript.

## Ready-for-upload artifacts

- Manuscript docx: `manuscript/NCC_Fiddler_Niche_submission_ready.docx`
- Figures: `figures/`
- Source data: `source_data/`
- Results: `results/`
- Analysis code: `analysis/`
- Package metadata: `README.md`, `CODE_AVAILABILITY.md`, `DATA_AVAILABILITY.md`, `DATAONE_PACKAGE_MANIFEST.csv`, `COMMENT_ALIGNMENT_SUMMARY.md`
