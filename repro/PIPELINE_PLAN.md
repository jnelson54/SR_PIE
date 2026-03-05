# Nature-Comms-Grade Reproducibility Plan

## 1) Freeze inputs

- Keep immutable raw archives in `data/raw/pasta/` and `data/raw/pasta_sonde/`.
- Track package IDs and revisions in manifest CSVs under `repro/manifests/`.
- Never edit raw files in place.

## 2) Deterministic environment

- Create one locked environment file (`environment.yml` or `requirements.txt` + pinned versions).
- Record Python and R versions.
- Add a single bootstrap command to recreate the environment.

## 3) Data build graph

- Stage A: unpack package ZIPs into `data/stage/package_id/...`.
- Stage B: standardize schema (timestamps, station names, QC flags, units).
- Stage C: analysis-ready tables (season x region x year for hydro anomalies; annual burrow summaries; food web model inputs).
- Stage D: figures/tables and manuscript stats.

Use a workflow engine (`snakemake`, `targets`, or `make`) so every output is generated from declared inputs.

## 4) Validation gates

- Add schema checks for each staged table (required columns, types, allowed ranges).
- Add assertions for expected year coverage per analysis panel.
- Add regression tests for key outputs (e.g., anomaly slopes and breakpoints).

## 5) Provenance and traceability

- Emit run metadata (`run_id`, git commit, timestamp, manifest hash, package checksums).
- Save intermediate and final artifacts under versioned output directories.
- Keep one machine-readable `results_manifest.json`.

## 6) CI and publication readiness

- CI job should run: environment build -> data audit -> core analysis subset -> tests.
- Tag release used for submission and archive to Zenodo.
- Include `repro/README.md` quick-start commands in the manuscript Data/Code Availability statement.
