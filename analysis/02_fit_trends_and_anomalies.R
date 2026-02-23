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
  "dplyr", "tidyr", "readr", "purrr", "tibble", "lmtest", "sandwich"
))

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  library(tibble)
  library(lmtest)
  library(sandwich)
})

root <- get_project_root()
in_file <- file.path(root, "analysis", "output", "sonde", "sonde_seasonal_means_junjul_augsep.csv")
out_dir <- file.path(root, "analysis", "output", "sonde")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(in_file)) {
  stop("Input seasonal data not found: ", in_file, "\nRun analysis/01_prepare_sonde_seasonal_data.R first.")
}

seasonal <- readr::read_csv(in_file, show_col_types = FALSE)

make_long <- function(df) {
  bind_rows(
    df %>%
      transmute(
        combined_site, year, season_bin, complete_bin,
        n_days = n_days_temp,
        variable = "temperature",
        raw_value = temperature
      ),
    df %>%
      transmute(
        combined_site, year, season_bin, complete_bin,
        n_days = n_days_salinity,
        variable = "salinity",
        raw_value = salinity
      ),
    df %>%
      transmute(
        combined_site, year, season_bin, complete_bin,
        n_days = n_days_conductivity,
        variable = "conductivity",
        raw_value = conductivity
      )
  )
}

choose_baseline_years <- function(years, pref_start = 2000L, pref_end = 2013L, min_pref_years = 8L, fallback_n = 5L) {
  years <- sort(unique(as.integer(years)))
  preferred <- years[years >= pref_start & years <= pref_end]

  if (length(preferred) >= min_pref_years) {
    return(list(
      years = preferred,
      rule = "fixed_2000_2013_window"
    ))
  }

  if (length(years) >= fallback_n) {
    fb <- years[seq_len(fallback_n)]
    return(list(
      years = fb,
      rule = "fallback_first_5_years"
    ))
  }

  list(
    years = years,
    rule = "fallback_all_available_years"
  )
}

fit_trend <- function(year, value) {
  ok <- is.finite(year) & is.finite(value)
  year <- as.numeric(year[ok])
  value <- as.numeric(value[ok])
  ord <- order(year)
  year <- year[ord]
  value <- value[ord]
  n <- length(year)

  if (n < 6) {
    return(tibble(
      n_years = n,
      start_year = ifelse(n > 0, min(year), NA_real_),
      end_year = ifelse(n > 0, max(year), NA_real_),
      slope_ols_decade = NA_real_,
      slope_ols_ci_lcl_decade = NA_real_,
      slope_ols_ci_ucl_decade = NA_real_,
      p_ols = NA_real_,
      p_newey_west = NA_real_,
      slope_newey_west_se_decade = NA_real_,
      slope_theil_sen_decade = NA_real_,
      slope_theil_sen_ci_lcl_decade = NA_real_,
      slope_theil_sen_ci_ucl_decade = NA_real_,
      kendall_tau = NA_real_,
      p_kendall = NA_real_,
      r_squared = NA_real_
    ))
  }

  model_df <- tibble(year = year, value = value)
  fit <- stats::lm(value ~ year, data = model_df)
  fit_sum <- summary(fit)
  coef_tab <- fit_sum$coefficients

  slope_year <- unname(coef_tab["year", "Estimate"])
  p_ols <- unname(coef_tab["year", "Pr(>|t|)"])
  r2 <- unname(fit_sum$r.squared)

  ci <- tryCatch(
    stats::confint(fit, parm = "year", level = 0.95),
    error = function(e) matrix(c(NA_real_, NA_real_), nrow = 1, dimnames = list("year", c("2.5 %", "97.5 %")))
  )
  ci_l <- ci["year", 1]
  ci_u <- ci["year", 2]

  lag_nw <- max(0, min(3, floor(n / 4)))
  nw <- tryCatch(
    lmtest::coeftest(
      fit,
      vcov. = sandwich::NeweyWest(fit, lag = lag_nw, prewhite = FALSE, adjust = TRUE)
    ),
    error = function(e) NULL
  )

  if (!is.null(nw) && "year" %in% rownames(nw)) {
    p_nw <- unname(nw["year", "Pr(>|t|)"])
    se_nw <- unname(nw["year", "Std. Error"])
  } else {
    p_nw <- NA_real_
    se_nw <- NA_real_
  }

  kendall <- tryCatch(
    stats::cor.test(year, value, method = "kendall", exact = FALSE),
    error = function(e) NULL
  )
  tau <- if (!is.null(kendall)) unname(kendall$estimate) else NA_real_
  p_tau <- if (!is.null(kendall)) kendall$p.value else NA_real_

  ts_slope <- theil_sen_slope(year, value)
  ts_ci <- bootstrap_theil_sen_ci(year, value, b = 2000, conf = 0.95, seed = 123)

  tibble(
    n_years = n,
    start_year = min(year),
    end_year = max(year),
    slope_ols_decade = slope_year * 10,
    slope_ols_ci_lcl_decade = ci_l * 10,
    slope_ols_ci_ucl_decade = ci_u * 10,
    p_ols = p_ols,
    p_newey_west = p_nw,
    slope_newey_west_se_decade = se_nw * 10,
    slope_theil_sen_decade = ts_slope * 10,
    slope_theil_sen_ci_lcl_decade = ts_ci[1] * 10,
    slope_theil_sen_ci_ucl_decade = ts_ci[2] * 10,
    kendall_tau = tau,
    p_kendall = p_tau,
    r_squared = r2
  )
}

seasonal_long <- make_long(seasonal) %>%
  mutate(
    combined_site = as.character(combined_site),
    season_bin = factor(season_bin, levels = c("Jun-Jul", "Aug-Sep"), ordered = TRUE),
    variable = factor(variable, levels = c("temperature", "salinity", "conductivity"), ordered = TRUE),
    include_for_trend = is.finite(raw_value) & n_days >= 10
  ) %>%
  arrange(variable, combined_site, season_bin, year)

baseline_defs <- seasonal_long %>%
  filter(include_for_trend) %>%
  group_by(combined_site, season_bin, variable) %>%
  group_modify(~{
    yrs <- sort(unique(.x$year))
    base <- choose_baseline_years(yrs)
    b_mean <- mean(.x$raw_value[.x$year %in% base$years], na.rm = TRUE)
    tibble(
      baseline_rule = base$rule,
      baseline_start_year = min(base$years),
      baseline_end_year = max(base$years),
      baseline_years = paste(base$years, collapse = ";"),
      baseline_n_years = length(base$years),
      baseline_mean = b_mean
    )
  }) %>%
  ungroup() %>%
  mutate(
    variable = as.character(variable),
    season_bin = as.character(season_bin)
  )

anomaly_series <- seasonal_long %>%
  left_join(baseline_defs, by = c("combined_site", "season_bin", "variable")) %>%
  mutate(
    anomaly = raw_value - baseline_mean,
    analysis_value_raw = raw_value,
    analysis_value_anomaly = anomaly
  ) %>%
  arrange(variable, combined_site, season_bin, year)

raw_trends <- anomaly_series %>%
  filter(include_for_trend, is.finite(analysis_value_raw)) %>%
  group_by(combined_site, season_bin, variable) %>%
  group_modify(~fit_trend(.x$year, .x$analysis_value_raw)) %>%
  ungroup() %>%
  mutate(series_type = "raw")

anom_trends <- anomaly_series %>%
  filter(include_for_trend, is.finite(analysis_value_anomaly)) %>%
  group_by(combined_site, season_bin, variable) %>%
  group_modify(~fit_trend(.x$year, .x$analysis_value_anomaly)) %>%
  ungroup() %>%
  mutate(series_type = "anomaly")

trend_results <- bind_rows(raw_trends, anom_trends) %>%
  mutate(
    variable = as.character(variable),
    season_bin = as.character(season_bin)
  ) %>%
  group_by(series_type, variable) %>%
  mutate(
    q_newey_west_bh = p.adjust(p_newey_west, method = "BH"),
    q_kendall_bh = p.adjust(p_kendall, method = "BH")
  ) %>%
  ungroup() %>%
  arrange(variable, combined_site, season_bin, series_type)

period_shift <- anomaly_series %>%
  filter(include_for_trend) %>%
  group_by(combined_site, season_bin, variable) %>%
  summarise(
    mean_pre2014 = safe_mean(raw_value[year <= 2013]),
    mean_post2014 = safe_mean(raw_value[year >= 2014]),
    delta_post_minus_pre = mean_post2014 - mean_pre2014,
    mean_anomaly_pre2014 = safe_mean(anomaly[year <= 2013]),
    mean_anomaly_post2014 = safe_mean(anomaly[year >= 2014]),
    delta_anomaly_post_minus_pre = mean_anomaly_post2014 - mean_anomaly_pre2014,
    n_years_pre2014 = sum(is.finite(raw_value[year <= 2013])),
    n_years_post2014 = sum(is.finite(raw_value[year >= 2014])),
    .groups = "drop"
  ) %>%
  mutate(
    variable = as.character(variable),
    season_bin = as.character(season_bin)
  ) %>%
  arrange(variable, combined_site, season_bin)

write_csv(baseline_defs, file.path(out_dir, "sonde_anomaly_baselines.csv"))
write_csv(anomaly_series, file.path(out_dir, "sonde_seasonal_anomaly_series.csv"))
write_csv(trend_results, file.path(out_dir, "sonde_trend_results.csv"))
write_csv(period_shift, file.path(out_dir, "sonde_period_shift_summary.csv"))

message("Wrote anomaly and trend outputs to: ", out_dir)
