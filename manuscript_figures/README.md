# manuscript_figures: code and inputs behind Figs 1-4, Supplementary Fig. 1 and Supplementary Tables 2 and 4

Added in archive version 1.1.0. Version 1.0.1 held the rendered figures and their source data
(`submission_20260421/`) but not the scripts that produce them.

## Run

```
Rscript manuscript_figures/run_all.R
```

The wrapper runs the five scripts below and then `verify_reproduction.R`, which compares the
regenerated numbers with the figure source data archived for the submitted manuscript
(`submission_20260421/source_data`) and stops with an error if any value that should be
unchanged differs by more than 1e-8.

| Display item | Script | Inputs | Main outputs |
|---|---|---|---|
| Fig. 1 | `analysis/study_area/make_fig1_map.py` (Python; run separately) | `usgs_imagery_pie_main.jpg`, `usgs_topo_pie_inset.jpg`, `fig1_basemaps.json`, `PIE_sampling_locations.csv` | `figures/Fig1_study_area_map.png` |
| Fig. 2 (input) | `analysis/hydrography/00_build_sonde_anomalies.R` | `High Tide Values.csv` | `pie_temp_sal_sonde.csv`, `pie_sonde_season_means_and_counts.csv` |
| Fig. 2 | `analysis/hydrography/analysis_pie_temp_sal_sonde.R` | `pie_temp_sal_sonde.csv` | `figures/pie_anomalies_panel.*`, `source_data/Figure1_hydrography_*.csv` |
| Supplementary Fig. 1 | `analysis/discharge/analysis_parker_discharge_usgs.R` | `parker_discharge_daily_selected_gauge.csv` (USGS 01101000) | `figures/Supplementary_Figure1_parker_discharge.*`, `results/discharge/` |
| Fig. 3, Supplementary Table 4 | `analysis/burrow/burrow_density_analysis.R` | `burrow_density.csv` | `figures/fig_burrow_panel.*`, `results/burrow/` |
| Fig. 4 | `analysis/plankton/fig_plankton_trends_nyt_with_fiddler.R` | `aggregated_contributions.csv`, `fiddler_crab_contributions_filtered.csv` | `figures/fig_plankton_trends_nyt_with_fiddler.*`, `source_data/Figure3_*.csv` |

File names under `source_data/` keep the numbering of an earlier draft (Figure1 = hydrography,
Figure2 = burrows, Figure3 = plankton). Manuscript numbering is Fig. 2, Fig. 3 and Fig. 4.

Fig. 1 is drawn by `python manuscript_figures/analysis/study_area/make_fig1_map.py` (Python 3 with
numpy, matplotlib and Pillow; not part of `run_all.R`). Its base maps are public-domain USGS
products from The National Map: the USGSImageryOnly base map (NAIP orthoimagery) for the main
map and the USGSTopo base map for the inset. Both images were retrieved on 2026-11-21 with the
ArcGIS REST export requests recorded in `fig1_basemaps.json` and are stored beside the script.
This figure replaces the map submitted with the first version of the manuscript, which was
composed in ArcGIS Pro on a commercial imagery basemap and has been removed from
`submission_20260421/figures`. Sampling locations are unchanged.

## What is and is not regenerated from raw data

- Hydrography: regenerated from the high-tide sonde extract. The extract itself was drawn from
  the EDI packages listed in `repro/manifests/full_estuarine_moored_sonde_packages.csv`; that
  extraction step is not scripted here.
- Discharge and burrow density: regenerated from the daily discharge record and the quadrat
  counts.
- Mud fiddler crab mixing model: `analysis/plankton/PIE_Mixing_Model/fiddler_crabs/mixsiar_fiddler_crabs.R`
  refits the model from the 20 crab isotope values, the end-member file and the discrimination
  file (about 15 minutes; needs JAGS). It is not part of `run_all.R` because MCMC output differs
  slightly between runs. A refit on 2026-11-21 (R 4.3.3, MixSIAR 3.1.12, JAGS 4.3.2) gave
  73.5 +/- 2.6% plankton (POM) from the full posterior; all
  Gelman-Rubin statistics were below 1.05 (`rerun_2026-11-21/`).
- Native-consumer mixing models: `aggregated_contributions.csv` holds the archived posterior
  summaries (mean, SD, 5th and 95th percentiles by species, station, year and source). The
  models are not refitted here.

## Differences from the submitted manuscript

Regenerated values match the archived figure source data exactly, with one documented exception.

Hydrography q-values. The submitted script applied the Benjamini-Hochberg adjustment within
each region-by-variable group, so the reported q-values equalled the unadjusted p-values. The
adjustment is now applied across the four tests: lower-estuary temperature q = 5.8e-5 (was
1.5e-5), lower-estuary conductivity q = 0.018 (was 0.009), upper-estuary temperature q = 0.032
(was 0.024), upper-estuary conductivity q = 0.237 (unchanged). Slopes, confidence intervals and
Fig. 2 are unchanged.

Two smaller changes: the burrow script sets a random seed so the bootstrap intervals in Fig. 3a
repeat exactly, and it draws Fig. 3b without the Wilson interval bars, as in the submitted
figure (the intervals are still written to `results/burrow/burrow_occupancy_summary.csv`).

Fonts depend on the machine. The scripts request Libre Franklin and fall back to Arial, so text
rendering can differ from the submitted figures while the plotted data are the same.

## Sensitivity of the hydrographic trends

`sensitivity/hydro_trend_sensitivity.R` refits the trends (a) as published, (b) without
season bins holding fewer than 20 high-tide records, (c) with one value per year, and (d) with
April-November data and a monthly climatology. Results are in
`sensitivity/hydro_trend_sensitivity.csv` (Supplementary Table 2).

## Software

R 4.3.3 with dplyr 1.1.4, tidyr, readr, purrr, stringr, lubridate, ggplot2 3.4.4, patchwork
1.2.0, mgcv 1.9.1, scales, janitor 2.2.1, optparse, zyp 0.11.1 and Kendall 2.2.2. The crab
mixing model also needs MixSIAR 3.1.12, R2jags 0.8.9, rjags 4.15 and JAGS 4.3.2. Fig. 1 was drawn
with Python 3.13, matplotlib 3.11 and Pillow 12.
