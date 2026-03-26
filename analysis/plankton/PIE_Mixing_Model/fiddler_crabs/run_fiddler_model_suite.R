#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  if (!requireNamespace("MixSIAR", quietly = TRUE)) {
    stop("Package 'MixSIAR' not installed. Install it first: install.packages('MixSIAR')")
  }
  library(MixSIAR)
  library(readr)
  library(dplyr)
  library(purrr)
  library(rlang)
  library(tibble)
  library(stringr)
})

args <- commandArgs(trailingOnly = TRUE)
default_folder <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
folder <- if (length(args) >= 1) args[1] else default_folder
folder <- normalizePath(folder, winslash = "/", mustWork = FALSE)

safe_short_path <- function(path) {
  tryCatch(utils::shortPathName(path), error = function(e) path)
}

build_script <- file.path(folder, "build_fiddler_model_suite_inputs.R")
if (!file.exists(build_script)) {
  stop("Missing build script: ", build_script)
}

cat("Building raw-derived source and TEF inputs...\n")
build_output <- system2(
  command = file.path(R.home("bin"), "Rscript.exe"),
  args = shQuote(c(build_script, folder)),
  stdout = TRUE,
  stderr = TRUE
)
build_status <- attr(build_output, "status")
if (!is.null(build_status) && build_status != 0) {
  cat(paste(build_output, collapse = "\n"), "\n")
  stop("Input build failed with status ", build_status)
}

mix_file <- file.path(folder, "fiddler_crabs.csv")
input_dir <- safe_short_path(file.path(folder, "model_suite_inputs"))
output_root <- file.path(folder, "model_suite_outputs")
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
output_root <- safe_short_path(output_root)

if (!file.exists(mix_file)) {
  stop("Missing consumer file: ", mix_file)
}

run_single_model <- function(model_id, iso_names, source_file, tef_file) {
  output_dir <- file.path(output_root, model_id)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  mix <- load_mix_data(
    filename = mix_file,
    iso_names = iso_names,
    factors = character(0),
    fac_random = logical(0),
    fac_nested = logical(0),
    cont_effects = character(0)
  )

  src_head <- read.csv(source_file, nrows = 5, check.names = FALSE)
  conc_dep_flag <- any(grepl("^(conc|Conc)", colnames(src_head)))

  sources <- load_source_data(
    filename = source_file,
    conc_dep = conc_dep_flag,
    data_type = "means",
    mix = mix
  )

  discr <- load_discr_data(
    filename = tef_file,
    mix = mix
  )

  resid_err <- FALSE
  process_err <- TRUE
  model_file <- file.path(output_dir, paste0(model_id, "_model.txt"))

  write_JAGS_model(
    filename = model_file,
    resid_err = resid_err,
    process_err = process_err,
    mix = mix,
    source = sources
  )

  cat("Running ", model_id, " with isotopes: ", paste(iso_names, collapse = ", "), "\n", sep = "")

  model_results <- run_model(
    run = "very long",
    mix = mix,
    source = sources,
    discr = discr,
    model_filename = model_file,
    alpha.prior = 1,
    resid_err = resid_err,
    process_err = process_err
  )

  old_wd <- getwd()
  setwd(output_dir)
  on.exit(setwd(old_wd), add = TRUE)

  output_JAGS(
    model_results,
    mix,
    sources,
    output_options = list(
      summary_save = TRUE,
      summary_name = paste0(model_id, "_summary_statistics"),
      sup_post = TRUE,
      plot_post_save_png = TRUE,
      plot_post_name = paste0(model_id, "_posterior_density"),
      sup_pairs = TRUE,
      plot_pairs_save_png = TRUE,
      plot_pairs_name = paste0(model_id, "_pairs_plot"),
      sup_xy = TRUE,
      plot_xy_save_png = TRUE,
      plot_xy_name = paste0(model_id, "_xy_plot"),
      gelman = TRUE,
      heidel = FALSE,
      geweke = TRUE,
      diag_save = TRUE,
      diag_name = paste0(model_id, "_diagnostics"),
      indiv_effect = FALSE,
      plot_post_save_pdf = FALSE,
      plot_pairs_save_pdf = FALSE,
      plot_xy_save_pdf = FALSE
    )
  )

  posterior <- as.data.frame(as.table(model_results$BUGSoutput$sims.list$p.global)) %>%
    dplyr::rename(draw = .data$Var1, src_idx = .data$Var2, contribution = .data$Freq) %>%
    dplyr::mutate(source = sources$source_names[as.numeric(.data$src_idx)])

  summary_stats <- posterior %>%
    dplyr::group_by(.data$source) %>%
    dplyr::summarise(
      mean_contribution = mean(.data$contribution),
      sd_contribution = stats::sd(.data$contribution),
      p05 = as.numeric(stats::quantile(.data$contribution, 0.05)),
      p50 = as.numeric(stats::quantile(.data$contribution, 0.50)),
      p95 = as.numeric(stats::quantile(.data$contribution, 0.95)),
      .groups = "drop"
    )

  bugs_summary <- model_results$BUGSoutput$summary
  pglobal_rows <- bugs_summary[grepl("^p.global\\[", rownames(bugs_summary)), , drop = FALSE]
  diag_stats <- tibble::tibble(
    source = sources$source_names,
    Rhat = pglobal_rows[, "Rhat"],
    n_eff = pglobal_rows[, "n.eff"]
  )

  mix_data <- readr::read_csv(mix_file, show_col_types = FALSE)
  years_label <- mix_data %>%
    dplyr::pull(.data$year) %>%
    stats::na.omit() %>%
    unique() %>%
    sort() %>%
    paste(collapse = ",")

  out <- summary_stats %>%
    dplyr::left_join(diag_stats, by = "source") %>%
    dplyr::mutate(
      model_id = model_id,
      isotopes = paste(iso_names, collapse = ","),
      n_consumers = nrow(mix_data),
      years = years_label,
      source_file = normalizePath(source_file, winslash = "/", mustWork = TRUE),
      tef_file = normalizePath(tef_file, winslash = "/", mustWork = TRUE)
    ) %>%
    dplyr::select(.data$model_id, .data$isotopes, .data$source, .data$mean_contribution, .data$sd_contribution, .data$p05, .data$p50, .data$p95, .data$Rhat, .data$n_eff, .data$n_consumers, .data$years, .data$source_file, .data$tef_file)

  readr::write_csv(out, file.path(output_dir, paste0(model_id, "_contributions.csv")))

  model_diag <- tibble::tibble(
    model_id = model_id,
    isotopes = paste(iso_names, collapse = ","),
    max_Rhat = max(diag_stats$Rhat, na.rm = TRUE),
    min_n_eff = min(diag_stats$n_eff, na.rm = TRUE)
  )
  readr::write_csv(model_diag, file.path(output_dir, paste0(model_id, "_diagnostic_summary.csv")))

  list(contributions = out, diagnostics = model_diag)
}

model_defs <- tibble::tribble(
  ~model_id, ~iso_names, ~source_file, ~tef_file,
  "rebuild_2iso_standard", "d13C,d34S", file.path(input_dir, "sources_rebuild_2iso.csv"), file.path(input_dir, "tef_standard.csv"),
  "rebuild_2iso_pathway", "d13C,d34S", file.path(input_dir, "sources_rebuild_2iso.csv"), file.path(input_dir, "tef_pathway.csv"),
  "rebuild_3iso_standard", "d13C,d15N,d34S", file.path(input_dir, "sources_rebuild_3iso.csv"), file.path(input_dir, "tef_standard.csv"),
  "rebuild_3iso_pathway", "d13C,d15N,d34S", file.path(input_dir, "sources_rebuild_3iso.csv"), file.path(input_dir, "tef_pathway.csv")
)

results <- purrr::pmap(
  model_defs,
  function(model_id, iso_names, source_file, tef_file) {
    run_single_model(
      model_id = model_id,
      iso_names = strsplit(iso_names, ",", fixed = TRUE)[[1]],
      source_file = source_file,
      tef_file = tef_file
    )
  }
)

all_contributions <- dplyr::bind_rows(purrr::map(results, "contributions"))
all_diagnostics <- dplyr::bind_rows(purrr::map(results, "diagnostics"))

readr::write_csv(model_defs, file.path(output_root, "model_suite_definitions.csv"))
readr::write_csv(all_contributions, file.path(output_root, "model_suite_contributions.csv"))
readr::write_csv(all_diagnostics, file.path(output_root, "model_suite_diagnostics.csv"))

cat("Completed fiddler model suite. Outputs written to:\n  ", output_root, "\n", sep = "")