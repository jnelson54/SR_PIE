#!/usr/bin/env Rscript

scripts <- c(
  file.path("analysis", "study_area", "make_study_area_map.R"),
  file.path("analysis", "hydrography", "analysis_pie_temp_sal_sonde.R"),
  file.path("analysis", "discharge", "analysis_parker_discharge_usgs.R"),
  file.path("analysis", "burrow", "burrow_density_analysis.R"),
  file.path("analysis", "plankton", "fig_plankton_trends_nyt_with_fiddler.R")
)

rscript <- file.path(R.home("bin"), "Rscript")

for (script in scripts) {
  args <- if (grepl("fig_plankton_trends_nyt_with_fiddler.R$", script)) {
    c(script, "--mix", file.path("analysis", "plankton", "aggregated_contributions.csv"))
  } else {
    script
  }
  message("Running: ", paste(args, collapse = " "))
  status <- system2(rscript, args = args)
  if (!identical(status, 0L)) {
    stop("Script failed: ", script)
  }
}

message("All analysis modules completed.")
