#!/usr/bin/env Rscript
# MixSIAR 2-isotope (d13C, d34S) model for fiddler crabs (3 sources)
# - Random factor = "num" (each crab individually; fac_random=TRUE)
# - Run length = "very long"
# - Sources are "means"; auto-detect conc. dependence from source header
# - Uses PIE_primary_producers.csv and TEF_PIE.csv

suppressPackageStartupMessages({
  if (!requireNamespace("MixSIAR", quietly = TRUE)) {
    stop("Package 'MixSIAR' not installed. Install it first: install.packages('MixSIAR')")
  }
  library(MixSIAR)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
})

# --------------------------
# 1) Inputs / CLI arguments
# --------------------------
args <- commandArgs(trailingOnly = TRUE)
default_folder <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
folder <- if (length(args) >= 1) args[1] else default_folder

# Normalize Windows path and handle spaces
folder <- normalizePath(folder, winslash = "/", mustWork = FALSE)

mix_file    <- file.path(folder, "fiddler_crabs.csv")
source_file <- file.path(folder, "PIE_primary_producers.csv")
tef_file    <- file.path(folder, "TEF_PIE.csv")

# Output directory
output_dir <- file.path(folder, "mixsiar_outputs")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cat("Mix file:    ", mix_file,    "\n")
cat("Source file: ", source_file, "\n")
cat("TEF file:    ", tef_file,    "\n")
cat("Output dir:  ", output_dir,  "\n")

# --------------------------
# 2) Basic sanity checks
# --------------------------
if (!file.exists(mix_file))    stop("Missing consumers file: ", mix_file)
if (!file.exists(source_file)) stop("Missing sources file:   ", source_file)
if (!file.exists(tef_file))    stop("Missing TEF file:       ", tef_file)

# Must have these columns in the consumers (mix) file
required_mix_cols <- c("d13C", "d34S", "num")
mix_head <- read.csv(mix_file, nrows = 5, check.names = FALSE)
missing_mix <- setdiff(required_mix_cols, colnames(mix_head))
if (length(missing_mix) > 0) {
  stop("Missing required columns in mix file: ", paste(missing_mix, collapse = ", "))
}

# Auto-detect concentration dependence from source file header
src_head <- read.csv(source_file, nrows = 5, check.names = FALSE)
has_conc <- any(grepl("^(conc|Conc)", colnames(src_head)))
conc_dep_flag <- isTRUE(has_conc)
cat("Detected concentration columns in sources? ", conc_dep_flag, "\n")

# --------------------------
# 3) Load MixSIAR structures
# --------------------------
iso_names <- c("d13C", "d34S")

mix <- load_mix_data(
  filename     = mix_file,
  iso_names    = iso_names,
  factors      = "num",
  fac_random   = TRUE,
  fac_nested   = FALSE,
  cont_effects = NULL
)

sources <- load_source_data(
  filename  = source_file,
  conc_dep  = conc_dep_flag,
  data_type = "means",
  mix       = mix
)

discr <- load_discr_data(
  filename = tef_file,
  mix      = mix
)

# Helpful check: make sure we really have 3 sources
if (length(sources$source_names) != 3) {
  stop("This script expects exactly 3 sources; found: ",
       paste(sources$source_names, collapse = ", "))
}
cat("Sources loaded: ", paste(sources$source_names, collapse = ", "), "\n")

# --------------------------------
# 4) Build and run the JAGS model
# --------------------------------
resid_err   <- FALSE
process_err <- TRUE

model_file  <- file.path(output_dir, "MixSIAR_model_2iso.txt")

write_JAGS_model(
  filename    = model_file,
  resid_err   = resid_err,
  process_err = process_err,
  mix         = mix,
  source      = sources
)

cat("Running model = 'very long' ...\n")
model_results <- run_model(
  run            = "very long",
  mix            = mix,
  source         = sources,
  discr          = discr,
  model_filename = model_file,
  alpha.prior    = 1,
  resid_err      = resid_err,
  process_err    = process_err
)

# --------------------------------
# 5) Output diagnostics & plots
# --------------------------------
folder_safe <- gsub("[^A-Za-z0-9]+", "_", basename(folder))
short_summary_name <- paste0(folder_safe, "_summary_statistics_2iso")
short_post_name    <- paste0(folder_safe, "_posterior_density_2iso")
short_pairs_name   <- paste0(folder_safe, "_pairs_plot_2iso")
short_xy_name      <- paste0(folder_safe, "_xy_plot_2iso")
short_diag_name    <- paste0(folder_safe, "_diagnostics_2iso")

old_wd <- getwd()
setwd(output_dir)
on.exit(setwd(old_wd), add = TRUE)

output_JAGS(
  model_results,
  mix,
  sources,
  output_options = list(
    summary_save         = TRUE,
    summary_name         = short_summary_name,
    sup_post             = TRUE,
    plot_post_save_png   = TRUE,
    plot_post_name       = short_post_name,
    sup_pairs            = TRUE,
    plot_pairs_save_png  = TRUE,
    plot_pairs_name      = short_pairs_name,
    sup_xy               = TRUE,
    plot_xy_save_png     = TRUE,
    plot_xy_name         = short_xy_name,
    gelman               = TRUE,
    heidel               = FALSE,
    geweke               = TRUE,
    diag_save            = TRUE,
    diag_name            = short_diag_name,
    indiv_effect         = FALSE,
    plot_post_save_pdf   = FALSE,
    plot_pairs_save_pdf  = FALSE,
    plot_xy_save_pdf     = FALSE
  )
)

cat("MixSIAR run complete. Outputs written to:\n  ", output_dir, "\n")

# ------------------------------------------------------------
# 6) Posterior source contributions
#    p.fac1 is indexed [MCMC draw, individual, source].
#    a) per-individual posterior summaries
#    b) per-draw mean across individuals (the population-level quantity reported)
#    c) posterior mean, SD and 5th/95th percentiles of (b)
# ------------------------------------------------------------
pf <- model_results$BUGSoutput$sims.list$p.fac1
if (is.null(pf)) stop("Expected p.fac1 in BUGS output (random factor 'num').")

posterior_samples <-
  as.data.frame(as.table(pf)) %>%
  dplyr::rename(draw = Var1, level = Var2, src_idx = Var3, Contr = Freq) %>%
  dplyr::mutate(
    draw   = as.integer(draw),
    num    = as.integer(level),
    Source = sources$source_names[as.integer(src_idx)]
  ) %>%
  dplyr::select(draw, num, Source, Contr)

individual_summary <- posterior_samples %>%
  dplyr::group_by(num, Source) %>%
  dplyr::summarise(
    mean_Contr = mean(Contr), sd_Contr = sd(Contr),
    p5_Contr = as.numeric(stats::quantile(Contr, 0.05)),
    p95_Contr = as.numeric(stats::quantile(Contr, 0.95)),
    .groups = "drop"
  ) %>%
  dplyr::left_join(
    readr::read_csv(mix_file, show_col_types = FALSE) %>% dplyr::select(num, year),
    by = "num"
  )

draw_means <- posterior_samples %>%
  dplyr::group_by(draw, Source) %>%
  dplyr::summarise(mean_Contr = mean(Contr), .groups = "drop")

population_summary <- draw_means %>%
  dplyr::group_by(Source) %>%
  dplyr::summarise(
    sd_Contr  = sd(mean_Contr),
    p5_Contr  = as.numeric(stats::quantile(mean_Contr, 0.05)),
    p95_Contr = as.numeric(stats::quantile(mean_Contr, 0.95)),
    mean_Contr = mean(mean_Contr),
    .groups = "drop"
  ) %>%
  dplyr::select(Source, mean_Contr, sd_Contr, p5_Contr, p95_Contr) %>%
  dplyr::mutate(n = dplyr::n_distinct(posterior_samples$num), years = "2023,2025")

readr::write_csv(individual_summary, file.path(output_dir, "fiddler_crab_individual_contributions.csv"))
readr::write_csv(draw_means, file.path(output_dir, "fiddler_crab_posterior_draw_means.csv"))
readr::write_csv(population_summary, file.path(output_dir, "fiddler_crab_contributions_summary.csv"))
cat("Saved individual, per-draw and population summaries to:\n  ", output_dir, "\n")
print(population_summary)

invisible(TRUE)
