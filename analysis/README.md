# Sonde Trend And Anomaly Analysis (R)

This workflow builds the temperature and salinity anomaly analysis from original PIE LTER sonde files.

## Site Rules

- Include and combine `IBYC` + `IB` as `LOWER_MAIN`
- Include and combine `MB` + `MR` as `MIDDLE_MAIN`
- Exclude `RR`, `RO`, and `RT1A`

## Seasonal Bins

- `Jun-Jul`
- `Aug-Sep`

## Statistical Outputs

- Seasonal means (raw values)
- Baseline-based anomalies (site x season x variable)
- Trend estimates:
  - OLS slope and 95% CI
  - Newey-West robust p-value
  - Theil-Sen slope with bootstrap 95% CI
  - Kendall tau trend test
  - BH-adjusted q-values

## Run

```powershell
"C:\Program Files\R\R-4.5.2\bin\Rscript.exe" analysis\run_all.R
```

## Key Outputs

- `analysis/output/sonde/sonde_seasonal_means_junjul_augsep.csv`
- `analysis/output/sonde/sonde_anomaly_baselines.csv`
- `analysis/output/sonde/sonde_seasonal_anomaly_series.csv`
- `analysis/output/sonde/sonde_trend_results.csv`
- `analysis/output/sonde/sonde_period_shift_summary.csv`
- `analysis/figures/figure_temperature_raw_anomaly.png`
- `analysis/figures/figure_salinity_raw_anomaly.png`
