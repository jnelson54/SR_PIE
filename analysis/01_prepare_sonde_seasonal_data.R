resolve_common_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- args[grepl("^--file=", args)]
  if (length(file_arg) > 0) {
    script_path <- normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = FALSE)
    candidate <- file.path(dirname(script_path), "_common.R")
    if (file.exists(candidate)) return(candidate)
  }
  "analysis/_common.R"
}

suppressPackageStartupMessages({
  source(resolve_common_path())
})

ensure_packages(c(
  "dplyr", "tidyr", "purrr", "readr", "stringr", "lubridate", "tibble"
))

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(tibble)
})

root <- get_project_root()
raw_dir <- file.path(root, "data", "raw", "pasta_sonde")
out_dir <- file.path(root, "analysis", "output", "sonde")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (!dir.exists(raw_dir)) {
  stop("Raw sonde directory not found: ", raw_dir)
}

read_csv_safe <- function(path) {
  tryCatch(
    readr::read_csv(
      path,
      show_col_types = FALSE,
      progress = FALSE,
      guess_max = 20000,
      na = c("", "NA", "NaN", "-9999", "-99999"),
      locale = readr::locale(encoding = "Latin1")
    ),
    error = function(e) {
      tryCatch(
        read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
        error = function(e2) NULL
      )
    }
  )
}

read_one_file <- function(path) {
  rel_path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  root_norm <- normalizePath(root, winslash = "/", mustWork = FALSE)
  if (startsWith(rel_path, root_norm)) {
    rel_path <- substr(rel_path, nchar(root_norm) + 1, nchar(rel_path))
  }

  df <- read_csv_safe(path)
  if (is.null(df)) {
    return(list(
      daily = tibble(),
      file_log = tibble(
        file = rel_path,
        status = "read_failed",
        rows = NA_integer_,
        parsed_datetimes = NA_integer_,
        site_code = NA_character_,
        combined_site = NA_character_,
        rows_after_filters = NA_integer_
      )
    ))
  }

  if (nrow(df) == 0) {
    return(list(
      daily = tibble(),
      file_log = tibble(
        file = rel_path,
        status = "empty_file",
        rows = 0L,
        parsed_datetimes = 0L,
        site_code = NA_character_,
        combined_site = NA_character_,
        rows_after_filters = 0L
      )
    ))
  }

  dt <- infer_datetime(df)
  parsed_dt <- sum(!is.na(dt))
  site_code <- infer_site_code(path, df)
  combined_site <- combined_site_from_code(site_code)

  if (is.na(combined_site)) {
    return(list(
      daily = tibble(),
      file_log = tibble(
        file = rel_path,
        status = "excluded_site",
        rows = as.integer(nrow(df)),
        parsed_datetimes = as.integer(parsed_dt),
        site_code = site_code,
        combined_site = NA_character_,
        rows_after_filters = 0L
      )
    ))
  }

  temp <- extract_numeric(df, c("^temp$", "temperature"))
  salinity <- extract_numeric(df, c("^salinity$", "calcsalinity"))
  conductivity <- extract_numeric(df, c("^spcond$", "^cond$", "conduct"))
  do_val <- extract_numeric(df, c("^do($|\\b|_)", "^odo", "dissolved[ _]?oxygen"))

  cond_med <- suppressWarnings(stats::median(conductivity, na.rm = TRUE))
  if (is.finite(cond_med) && cond_med > 200) {
    conductivity <- conductivity / 1000
  }

  date <- as.Date(dt, tz = "UTC")
  month <- lubridate::month(date)
  year <- lubridate::year(date)

  cleaned <- tibble(
    datetime = dt,
    date = date,
    year = year,
    month = month,
    site_code = site_code,
    combined_site = combined_site,
    temperature = temp,
    salinity = salinity,
    conductivity = conductivity,
    dissolved_oxygen = do_val
  ) %>%
    mutate(
      temperature = ifelse(temperature < -5 | temperature > 45, NA_real_, temperature),
      salinity = ifelse(salinity < 0 | salinity > 45, NA_real_, salinity),
      conductivity = ifelse(conductivity < 0 | conductivity > 80, NA_real_, conductivity),
      dissolved_oxygen = ifelse(dissolved_oxygen < 0 | dissolved_oxygen > 30, NA_real_, dissolved_oxygen)
    ) %>%
    filter(!is.na(date), month %in% 6:9)

  if (nrow(cleaned) == 0) {
    return(list(
      daily = tibble(),
      file_log = tibble(
        file = rel_path,
        status = "no_rows_after_filters",
        rows = as.integer(nrow(df)),
        parsed_datetimes = as.integer(parsed_dt),
        site_code = site_code,
        combined_site = combined_site,
        rows_after_filters = 0L
      )
    ))
  }

  daily <- cleaned %>%
    group_by(combined_site, site_code, date, year, month) %>%
    summarise(
      temperature = safe_mean(temperature),
      salinity = safe_mean(salinity),
      conductivity = safe_mean(conductivity),
      dissolved_oxygen = safe_mean(dissolved_oxygen),
      n_obs_total = dplyr::n(),
      n_temp_obs = sum(!is.na(temperature)),
      n_salinity_obs = sum(!is.na(salinity)),
      n_conductivity_obs = sum(!is.na(conductivity)),
      .groups = "drop"
    )

  list(
    daily = daily,
    file_log = tibble(
      file = rel_path,
      status = "ok",
      rows = as.integer(nrow(df)),
      parsed_datetimes = as.integer(parsed_dt),
      site_code = site_code,
      combined_site = combined_site,
      rows_after_filters = as.integer(nrow(cleaned))
    )
  )
}

csv_files <- list.files(raw_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
csv_files <- csv_files[grepl("YSI", basename(csv_files), ignore.case = TRUE)]
csv_files <- csv_files[!grepl("^knb-lter-pie\\.", basename(csv_files), ignore.case = TRUE)]

if (length(csv_files) == 0) {
  stop("No sonde CSV files found under: ", raw_dir)
}

message("Found ", length(csv_files), " sonde CSV files.")
parsed <- purrr::map(csv_files, read_one_file)

file_log <- bind_rows(purrr::map(parsed, "file_log")) %>%
  arrange(file)

daily_by_file <- bind_rows(purrr::map(parsed, "daily"))
if (nrow(daily_by_file) == 0) {
  stop("No daily sonde data produced after filters.")
}

daily_combined <- daily_by_file %>%
  group_by(combined_site, date, year, month) %>%
  summarise(
    source_sites = paste(sort(unique(site_code)), collapse = ";"),
    n_site_days = dplyr::n(),
    temperature = safe_mean(temperature),
    salinity = safe_mean(salinity),
    conductivity = safe_mean(conductivity),
    dissolved_oxygen = safe_mean(dissolved_oxygen),
    n_obs_total = sum(n_obs_total, na.rm = TRUE),
    n_temp_obs = sum(n_temp_obs, na.rm = TRUE),
    n_salinity_obs = sum(n_salinity_obs, na.rm = TRUE),
    n_conductivity_obs = sum(n_conductivity_obs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    season_bin = case_when(
      month %in% c(6, 7) ~ "Jun-Jul",
      month %in% c(8, 9) ~ "Aug-Sep",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(season_bin))

monthly_coverage <- daily_combined %>%
  group_by(combined_site, year, month) %>%
  summarise(
    first_date = min(date, na.rm = TRUE),
    last_date = max(date, na.rm = TRUE),
    n_days_any = dplyr::n(),
    n_days_temp = sum(!is.na(temperature)),
    n_days_salinity = sum(!is.na(salinity)),
    n_days_conductivity = sum(!is.na(conductivity)),
    .groups = "drop"
  ) %>%
  arrange(combined_site, year, month)

seasonal_means <- daily_combined %>%
  group_by(combined_site, year, season_bin) %>%
  summarise(
    first_date = min(date, na.rm = TRUE),
    last_date = max(date, na.rm = TRUE),
    months_present = paste(sort(unique(month)), collapse = ";"),
    n_months_present = n_distinct(month),
    complete_bin = n_months_present == 2,
    n_days_any = dplyr::n(),
    n_days_temp = sum(!is.na(temperature)),
    n_days_salinity = sum(!is.na(salinity)),
    n_days_conductivity = sum(!is.na(conductivity)),
    temperature = safe_mean(temperature),
    salinity = safe_mean(salinity),
    conductivity = safe_mean(conductivity),
    dissolved_oxygen = safe_mean(dissolved_oxygen),
    .groups = "drop"
  ) %>%
  arrange(combined_site, year, season_bin)

seasonal_accounting <- seasonal_means %>%
  transmute(
    combined_site,
    year,
    season_bin,
    first_date,
    last_date,
    months_present,
    complete_bin,
    n_days_any,
    n_days_temp,
    n_days_salinity,
    n_days_conductivity,
    has_temperature = n_days_temp > 0,
    has_salinity = n_days_salinity > 0,
    has_conductivity = n_days_conductivity > 0
  )

write_csv(file_log, file.path(out_dir, "file_parse_log.csv"))
write_csv(daily_by_file, file.path(out_dir, "sonde_daily_by_source_file.csv"))
write_csv(daily_combined, file.path(out_dir, "sonde_daily_combined_sites.csv"))
write_csv(monthly_coverage, file.path(out_dir, "sonde_monthly_coverage_combined_sites.csv"))
write_csv(seasonal_means, file.path(out_dir, "sonde_seasonal_means_junjul_augsep.csv"))
write_csv(seasonal_accounting, file.path(out_dir, "sonde_site_year_bin_variable_accounting.csv"))

message("Wrote outputs to: ", out_dir)
message("Files parsed (ok): ", sum(file_log$status == "ok"), "/", nrow(file_log))
