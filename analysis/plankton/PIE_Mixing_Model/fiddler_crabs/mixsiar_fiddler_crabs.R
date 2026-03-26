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
# 6) Tidy per-crab (num) posterior source contributions
#    -> summary by crab x source (mean, sd, 5th, 95th)
#    -> join back consumer metadata for convenience
# ------------------------------------------------------------
pf <- model_results$BUGSoutput$sims.list$p.fac1
if (is.null(pf)) stop("Expected p.fac1 in BUGS output (random factor 'num').")

posterior_samples <-
  as.data.frame(as.table(pf)) %>%
  dplyr::rename(level = Var1, draw = Var2, src_idx = Var3, Contr = Freq) %>%
  dplyr::mutate(
    num    = as.numeric(level),
    Source = sources$source_names[as.numeric(src_idx)]
  ) %>%
  dplyr::select(num, Source, draw, Contr)

summary_contr <-
  posterior_samples %>%
  dplyr::group_by(num, Source) %>%
  dplyr::summarise(
    mean_Contr = mean(Contr, na.rm = TRUE),
    sd_Contr   = sd(Contr,   na.rm = TRUE),
    p5_Contr   = as.numeric(stats::quantile(Contr, 0.05, na.rm = TRUE)),
    p95_Contr  = as.numeric(stats::quantile(Contr, 0.95, na.rm = TRUE)),
    .groups    = "drop"
  )

mix_meta <- readr::read_csv(mix_file, show_col_types = FALSE)
meta_cols <- intersect(colnames(mix_meta),
                       c("num","site","region","year","animal","species","species_code","iso","tef","common_name"))
mix_meta <- mix_meta %>% dplyr::mutate(num = as.numeric(num)) %>%
  dplyr::select(dplyr::all_of(meta_cols))

summary_contr <- summary_contr %>%
  dplyr::left_join(mix_meta, by = "num")

if ("common_name" %in% names(summary_contr) && !"species_code" %in% names(summary_contr)) {
  summary_contr <- summary_contr %>%
    dplyr::mutate(species_code = common_name)
}

if (!"region" %in% names(summary_contr)) {
  summary_contr <- summary_contr %>%
    dplyr::mutate(region = "lower")
}

if (!"iso" %in% names(summary_contr)) {
  summary_contr <- summary_contr %>%
    dplyr::mutate(iso = "2")
}

if (!"tef" %in% names(summary_contr)) {
  summary_contr <- summary_contr %>%
    dplyr::mutate(tef = "x1")
}

if (!"animal" %in% names(summary_contr)) {
  summary_contr <- summary_contr %>%
    dplyr::mutate(animal = "mud_fiddler")
}

# Save a descriptive file name AND a compatibility export
desc_csv <- file.path(output_dir, paste0(folder_safe, "_source_contributions_summary_2iso.csv"))
readr::write_csv(summary_contr, desc_csv)
cat("Saved source-contribution summary CSV:\n  ", desc_csv, "\n")

# A simple, generic name some downstream scripts expect:
compat_csv <- file.path(output_dir, "fiddler_crab_contributions.csv")
readr::write_csv(summary_contr, compat_csv)
cat("Also saved (compat):\n  ", compat_csv, "\n")

invisible(TRUE)
