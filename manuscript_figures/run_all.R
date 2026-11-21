#!/usr/bin/env Rscript
# Regenerate the analyses and figures reported in the manuscript (Figs 2-4, Supplementary Fig. 1,
# Supplementary Table 4) and check the numerical outputs against the archived figure source data.
#
# Usage, from the repository root or from manuscript_figures/:
#   Rscript manuscript_figures/run_all.R
#
# Outputs are written to manuscript_figures/results, manuscript_figures/source_data and
# manuscript_figures/figures. See manuscript_figures/README.md.

get_script_dir <- function() {
  matched <- grep("--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(matched) == 0) return(normalizePath(getwd(), winslash = "/"))
  normalizePath(dirname(sub("--file=", "", matched[1])), winslash = "/")
}
root <- get_script_dir()
rscript <- file.path(R.home("bin"), "Rscript")

steps <- list(
  list("Fig. 2 input: seasonal sonde anomalies", "analysis/hydrography/00_build_sonde_anomalies.R", character()),
  list("Fig. 2: hydrography trends",             "analysis/hydrography/analysis_pie_temp_sal_sonde.R", character()),
  list("Supplementary Fig. 1: discharge",        "analysis/discharge/analysis_parker_discharge_usgs.R", character()),
  list("Fig. 3 and Supplementary Table 4: burrows", "analysis/burrow/burrow_density_analysis.R", character()),
  list("Fig. 4: plankton (POM) contributions",   "analysis/plankton/fig_plankton_trends_nyt_with_fiddler.R",
       c("--mix", file.path(root, "analysis", "plankton", "aggregated_contributions.csv"))),
  list("Check against archived source data",     "verify_reproduction.R", character())
)

for (s in steps) {
  message("\n== ", s[[1]], " ==")
  status <- system2(rscript, c(shQuote(file.path(root, s[[2]])), shQuote(s[[3]])))
  if (!identical(status, 0L)) stop("Step failed: ", s[[2]])
}
message("\nAll steps completed.")
