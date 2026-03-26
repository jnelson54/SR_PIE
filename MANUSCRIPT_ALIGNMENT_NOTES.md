# Manuscript Alignment Notes

The current synchronized manuscript text source is `manuscript/NCC_Fiddler_Niche_submission_ready_2026-03-24.txt`.

The layout manuscript `manuscript/NCC Fiddler_Niche_2026-03-05.docx` has not been directly edited in this workspace, so that document should be refreshed from the synchronized text before final journal upload.

Current rerun status by module:

- Hydrography
  - Lower-estuary temperature and conductivity trends reproduce as strong positive trends.
  - The rerun lower-estuary conductivity q-value is `0.00150`, not the manuscript's `0.019`.

- Freshwater discharge
  - The rerun discharge outputs closely match the manuscript text.
  - Monthly anomaly trend: `-0.116 m3 s-1 decade-1`, `q = 0.00370`.
  - Annual mean trend: `-0.171 m3 s-1 decade-1`, `q = 0.199`.

- Burrow density
  - The rerun log-linear annual trend is `40.45%` per year with `q = 0.00151`.
  - The rerun Kendall trend is `tau = 0.759` with `q = 0.00230`.
  - Occupancy is `0.50` in 2014 and `1.00` by 2019, but the raw repeated-transect series also reaches `1.00` in 2016 before dipping in 2017-2018.

- Food web / MixSIAR
  - The native consumer time series in the packaged data run from `2009-2020`, not `2009-2024`.
  - After rerun with native consumers only in the regional summary, middle estuary increases significantly, but lower estuary and all four species are non-significant after BH correction.
  - The packaged mud fiddler MixSIAR input has been updated to `n = 20` (samples from `2023` and `2025`).
  - The canonical 20-crab rerun summary is `73.37%` POM, `25.92%` Spartina, and `0.71%` benthic microalgae.
  - Figure 3 source-data tables and the fiddler reference band have been aligned to that canonical rerun.

The package-level figure summaries, source-data tables, and the synchronized manuscript text now reflect the current rerun values. The remaining publication step is to propagate those same revisions into the final formatted manuscript document.
