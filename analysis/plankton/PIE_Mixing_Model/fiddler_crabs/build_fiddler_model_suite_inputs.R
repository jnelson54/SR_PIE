#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(rlang)
  library(stringr)
  library(purrr)
  library(tibble)
})

safe_write_csv <- function(data, path) {
  target_dir <- dirname(path)
  target_name <- basename(path)

  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  }

  target_dir_short <- tryCatch(utils::shortPathName(target_dir), error = function(e) target_dir)
  target_path <- file.path(target_dir_short, target_name)

  if (file.exists(target_path)) {
    unlink(target_path, force = TRUE)
  }

  utils::write.csv(data, target_path, row.names = FALSE, na = "")
  invisible(target_path)
}

args <- commandArgs(trailingOnly = TRUE)
default_folder <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
folder <- if (length(args) >= 1) args[1] else default_folder
folder <- normalizePath(folder, winslash = "/", mustWork = FALSE)

default_raw <- normalizePath(
  file.path(folder, "../../../../../Data_Analysis/HTL-PIE-YearlyIsotopeSurvey (1).csv"),
  winslash = "/",
  mustWork = FALSE
)
raw_file <- if (length(args) >= 2) args[2] else default_raw
raw_file <- normalizePath(raw_file, winslash = "/", mustWork = FALSE)

output_dir <- file.path(folder, "model_suite_inputs")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(raw_file)) {
  stop("Missing raw isotope survey file: ", raw_file)
}

one_step_means <- c(d15N = 3.0, d13C = 1.0, d34S = 0.6)
one_step_sds <- c(d15N = 1.0, d13C = 0.5, d34S = 0.2)

lower_sites <- c("HTL-PR-P10.1-OTL", "HTL-SO-S3.0-IBYC")
marsh_site <- "HTL-PR-P10.1-OTL"

normalize_common_name <- function(x) {
  x %>%
    stringr::str_squish() %>%
    stringr::str_to_lower()
}

raw <- readr::read_csv(raw_file, show_col_types = FALSE) %>%
  dplyr::mutate(
    Common_name_norm = normalize_common_name(Common_name),
    Species_norm = normalize_common_name(Species)
  )

source_rows <- raw %>%
  dplyr::filter(Site %in% lower_sites) %>%
  dplyr::transmute(
    Date,
    Site,
    Common_name = Common_name_norm,
    Species = Species_norm,
    d13C = `13C`,
    d15N = `15N`,
    d34S = `34S`
  ) %>%
  dplyr::filter(!is.na(d13C)) %>%
  dplyr::filter(!(Common_name == "benthic microalgae" & d13C > 0)) %>%
  dplyr::mutate(
    source_group = dplyr::case_when(
      Common_name == "benthic microalgae" ~ "benthic microalgae",
      Common_name == "zooplankton" ~ "pelagic zooplankton",
      Site == marsh_site & Common_name %in% c("salt water cord grass", "saltmeadow grass") ~ "marsh plants",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(source_group))

required_groups <- c("benthic microalgae", "pelagic zooplankton", "marsh plants")

build_source_summary <- function(data, iso_names) {
  required_cols <- iso_names
  data %>%
    dplyr::filter(dplyr::if_all(dplyr::all_of(required_cols), ~ !is.na(.x))) %>%
    dplyr::group_by(.data$source_group) %>%
    dplyr::summarise(
      Meand15N = mean(.data$d15N),
      SDd15N = stats::sd(.data$d15N),
      Meand13C = mean(.data$d13C),
      SDd13C = stats::sd(.data$d13C),
      Meand34S = mean(.data$d34S),
      SDd34S = stats::sd(.data$d34S),
      n = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      Concd15N = dplyr::case_when(
        .data$source_group == "benthic microalgae" ~ 8.0,
        .data$source_group == "pelagic zooplankton" ~ 8.0,
        .data$source_group == "marsh plants" ~ 1.7
      ),
      Concd13C = dplyr::case_when(
        .data$source_group == "benthic microalgae" ~ 42.6,
        .data$source_group == "pelagic zooplankton" ~ 42.0,
        .data$source_group == "marsh plants" ~ 54.0
      ),
      Concd34S = dplyr::case_when(
        .data$source_group == "benthic microalgae" ~ 0.6,
        .data$source_group == "pelagic zooplankton" ~ 0.6,
        .data$source_group == "marsh plants" ~ 1.0
      )
    ) %>%
    dplyr::mutate(Sources = .data$source_group) %>%
    dplyr::select(-dplyr::all_of("source_group")) %>%
    dplyr::select(dplyr::all_of(c("Sources", "Meand15N", "SDd15N", "Concd15N", "Meand13C", "SDd13C", "Concd13C", "Meand34S", "SDd34S", "Concd34S", "n"))) %>%
    dplyr::arrange(match(.data$Sources, required_groups))
}

source_2iso <- build_source_summary(source_rows, c("d13C", "d34S"))
source_3iso <- build_source_summary(source_rows, c("d13C", "d15N", "d34S"))

if (!all(required_groups %in% source_2iso$Sources)) {
  stop("Two-isotope rebuild is missing one or more required source groups.")
}

if (!all(required_groups %in% source_3iso$Sources)) {
  stop("Three-isotope rebuild is missing one or more required source groups.")
}

build_tef <- function(two_step_sources = character(0)) {
  tibble::tibble(
    Sources = required_groups,
    steps = ifelse(required_groups %in% two_step_sources, 2, 1)
  ) %>%
    dplyr::mutate(
      Meand15N = .data$steps * one_step_means[["d15N"]],
      SDd15N = sqrt(.data$steps) * one_step_sds[["d15N"]],
      Meand13C = .data$steps * one_step_means[["d13C"]],
      SDd13C = sqrt(.data$steps) * one_step_sds[["d13C"]],
      Meand34S = .data$steps * one_step_means[["d34S"]],
      SDd34S = sqrt(.data$steps) * one_step_sds[["d34S"]],
      n = 100
    ) %>%
    dplyr::select(-dplyr::all_of("steps"))
  }

tef_standard <- build_tef()
tef_pathway <- build_tef(c("benthic microalgae", "marsh plants"))

audit <- source_rows %>%
  dplyr::mutate(
    iso_set = dplyr::case_when(
      !is.na(d13C) & !is.na(d15N) & !is.na(d34S) ~ "3iso",
      !is.na(d13C) & !is.na(d34S) ~ "2iso",
      TRUE ~ "other"
    )
  ) %>%
  dplyr::filter(iso_set %in% c("2iso", "3iso")) %>%
  dplyr::count(source_group, Site, iso_set, name = "n") %>%
  dplyr::arrange(match(source_group, required_groups), Site, iso_set)

rules <- tibble::tribble(
  ~item, ~value,
  "raw_file", raw_file,
  "lower_sites", paste(lower_sites, collapse = "; "),
  "marsh_site", marsh_site,
  "pelagic_proxy", "zooplankton",
  "marsh_proxy", "salt water cord grass + saltmeadow grass at OTL",
  "excluded_rows", "benthic microalgae rows with d13C > 0 excluded as obvious outlier",
  "standard_tef", "all sources 1 trophic step",
  "pathway_tef", "benthic microalgae + marsh plants 2 steps; pelagic zooplankton 1 step"
)

safe_write_csv(source_rows, file.path(output_dir, "source_rows_rebuild.csv"))
safe_write_csv(source_2iso, file.path(output_dir, "sources_rebuild_2iso.csv"))
safe_write_csv(source_3iso, file.path(output_dir, "sources_rebuild_3iso.csv"))
safe_write_csv(tef_standard, file.path(output_dir, "tef_standard.csv"))
safe_write_csv(tef_pathway, file.path(output_dir, "tef_pathway.csv"))
safe_write_csv(audit, file.path(output_dir, "input_audit.csv"))
safe_write_csv(rules, file.path(output_dir, "input_rules.csv"))

cat("Built fiddler model suite inputs in:\n  ", output_dir, "\n", sep = "")