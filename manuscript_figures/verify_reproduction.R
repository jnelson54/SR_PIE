#!/usr/bin/env Rscript
# Compare regenerated numerical outputs with the figure source data archived with the
# submitted manuscript (../submission_20260421/source_data). Exits with an error if any
# value that should be unchanged differs by more than the tolerance.

get_script_dir <- function() {
  matched <- grep("--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(matched) == 0) return(normalizePath(getwd(), winslash = "/"))
  normalizePath(dirname(sub("--file=", "", matched[1])), winslash = "/")
}
root <- get_script_dir()
ref_dir <- file.path(root, "..", "submission_20260421", "source_data")
tol <- 1e-8

# reference file, regenerated file, columns expected to differ (documented in README)
pairs <- list(
  c("Fig2_hydro_anomalies.csv",        "source_data/Figure1_hydrography_anomalies.csv", ""),
  c("Fig2_hydro_trends.csv",           "source_data/Figure1_hydrography_trends.csv", "q_value,sig_q"),
  c("Fig3_burrow_annual.csv",          "results/burrow/burrow_annual_summary.csv", ""),
  c("Fig3_burrow_occupancy.csv",       "results/burrow/burrow_occupancy_summary.csv", ""),
  c("Fig3_burrow_trends.csv",          "results/burrow/burrow_trend_summary.csv", ""),
  c("Fig4_plankton_region_trends.csv", "source_data/Figure3_plankton_region_trends.csv", ""),
  c("Fig4_plankton_region_years.csv",  "source_data/Figure3_plankton_region_year_summary.csv", ""),
  c("Fig4_plankton_species_trends.csv","source_data/Figure3_plankton_species_trends.csv", ""),
  c("Fig4_plankton_species_years.csv", "source_data/Figure3_plankton_species_year_summary.csv", ""),
  c("SuppFig1_discharge_anomalies.csv","results/discharge/parker_discharge_monthly_anomalies.csv", ""),
  c("SuppFig1_discharge_prepost.csv",  "results/discharge/parker_discharge_prepost2014_tests.csv", ""),
  c("SuppFig1_discharge_trends.csv",   "results/discharge/parker_discharge_trend_summary.csv", ""),
  c("Fig4_fiddler_mixsiar.csv",        "source_data/Figure3_fiddler_mixsiar_summary.csv", "")
)
# column names that changed between the archived source data and the script outputs
rename_map <- list(median_contr = c("median_plankton_proportion"),
                   mean_contr = c("mean_plankton_proportion", "mean_proportion"),
                   sd_contr = c("sd_proportion"))

report <- list(); failed <- FALSE
for (p in pairs) {
  ref <- read.csv(file.path(ref_dir, p[1]), check.names = FALSE)
  new <- read.csv(file.path(root, p[2]), check.names = FALSE)
  skip <- strsplit(p[3], ",")[[1]]
  worst <- 0; n_cols <- 0; status <- "identical"
  if (nrow(ref) != nrow(new)) { status <- sprintf("ROW COUNT DIFFERS (%d vs %d)", nrow(ref), nrow(new)); failed <- TRUE }
  else for (col in names(ref)) {
    new_col <- if (col %in% names(new)) col else if (col %in% names(rename_map)) intersect(rename_map[[col]], names(new))[1] else NA
    if (is.na(new_col) || col %in% skip) next
    a <- ref[[col]]; b <- new[[new_col]]; n_cols <- n_cols + 1
    if (is.numeric(a) && is.numeric(b)) {
      if (any(is.na(a) != is.na(b))) { status <- paste("NA PATTERN DIFFERS:", col); failed <- TRUE; next }
      d <- suppressWarnings(max(abs(a - b), na.rm = TRUE)); if (!is.finite(d)) d <- 0
      worst <- max(worst, d)
      if (d > tol) { status <- paste("VALUES DIFFER:", col); failed <- TRUE }
    } else if (!identical(as.character(a), as.character(b))) { status <- paste("TEXT DIFFERS:", col); failed <- TRUE }
  }
  if (status == "identical" && length(skip)) status <- paste0("identical except ", paste(skip, collapse = ", "), " (see README)")
  report[[length(report) + 1]] <- data.frame(archived_source_data = p[1], regenerated = p[2], columns_compared = n_cols,
                                             max_abs_difference = signif(worst, 3), result = status)
}
report <- do.call(rbind, report)
print(report, row.names = FALSE, right = FALSE)
write.csv(report, file.path(root, "results", "reproduction_check.csv"), row.names = FALSE)

if (failed) stop("Reproduction check failed: see table above.")
cat("\nReproduction check passed.\n")
