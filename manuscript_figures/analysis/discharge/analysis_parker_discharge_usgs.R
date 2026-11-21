#!/usr/bin/env Rscript

install_if_missing <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    install.packages(missing, repos = "https://cloud.r-project.org")
  }
}

install_if_missing(c("readr", "dplyr", "tidyr", "ggplot2", "scales", "lubridate", "purrr", "Kendall", "zyp", "patchwork"))

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(lubridate)
  library(purrr)
  library(patchwork)
})

get_script_dir <- function() {
  file_arg <- "--file="
  matched <- grep(file_arg, commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(matched) == 0) {
    return(normalizePath(getwd(), winslash = "/", mustWork = TRUE))
  }
  normalizePath(dirname(sub(file_arg, "", matched[1])), winslash = "/", mustWork = TRUE)
}

script_dir <- get_script_dir()
repo_root <- normalizePath(file.path(script_dir, "..", ".."), winslash = "/", mustWork = TRUE)
results_dir <- file.path(repo_root, "results", "discharge")
figures_dir <- file.path(repo_root, "figures")
source(file.path(repo_root, "analysis", "common", "theme_nyt.R"))

if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)
if (!dir.exists(figures_dir)) dir.create(figures_dir, recursive = TRUE)

CFS_TO_CMS <- 0.028316846592
site_no <- "01101000"
station_nm <- "Parker River at Byfield, MA"
month_levels <- 3:11
month_labels <- month.abb[month_levels]

sig_label <- function(p) {
  case_when(
    !is.finite(p) ~ NA_character_,
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    TRUE ~ "ns"
  )
}

theil_sen <- function(y, t) {
  n <- length(y)
  if (n < 2) return(NA_real_)
  idx <- combn(seq_len(n), 2)
  slopes <- (y[idx[2, ]] - y[idx[1, ]]) / (t[idx[2, ]] - t[idx[1, ]])
  median(slopes, na.rm = TRUE)
}

extract_named_num <- function(x, keys) {
  nm <- names(x)
  if (is.null(nm)) return(NA_real_)
  idx <- match(keys, nm)
  idx <- idx[!is.na(idx)][1]
  if (length(idx) == 0 || is.na(idx)) return(NA_real_)
  suppressWarnings(as.numeric(x[[idx]]))
}

extract_p_from_zyp <- function(z_obj) {
  if (is.list(z_obj)) {
    vals <- c(
      suppressWarnings(as.numeric(z_obj$p.value)),
      suppressWarnings(as.numeric(z_obj$p_value)),
      suppressWarnings(as.numeric(z_obj$pval)),
      suppressWarnings(as.numeric(z_obj$sig)),
      suppressWarnings(as.numeric(z_obj$sl))
    )
    vals <- vals[is.finite(vals)]
    if (length(vals) > 0) return(vals[[1]])
    return(NA_real_)
  }
  if (is.atomic(z_obj)) {
    return(extract_named_num(z_obj, c("p.value", "p_value", "pval", "sig", "sl")))
  }
  NA_real_
}

calc_trend <- function(y, t) {
  ok <- is.finite(y) & is.finite(t)
  y <- y[ok]
  t <- t[ok]

  fallback_p <- function(y_in) {
    if (requireNamespace("Kendall", quietly = TRUE)) {
      return(Kendall::MannKendall(y_in)$sl)
    }
    NA_real_
  }

  if (length(y) < 3) {
    return(list(
      slope = NA_real_,
      intercept = NA_real_,
      p_value = NA_real_,
      method = "Insufficient data"
    ))
  }

  if (requireNamespace("zyp", quietly = TRUE)) {
    z <- try(zyp::zyp.trend.vector(y, t, method = "yuepilon"), silent = TRUE)
    if (!inherits(z, "try-error")) {
      slope <- if (is.list(z)) suppressWarnings(as.numeric(z$trend)) else extract_named_num(z, c("trend", "slope"))
      intercept <- if (is.list(z)) suppressWarnings(as.numeric(z$intercept)) else extract_named_num(z, c("intercept"))
      p_value <- extract_p_from_zyp(z)
      if (!is.finite(slope)) slope <- theil_sen(y, t)
      if (!is.finite(intercept)) intercept <- median(y - slope * t, na.rm = TRUE)
      if (!is.finite(p_value)) p_value <- fallback_p(y)
      return(list(
        slope = slope,
        intercept = intercept,
        p_value = p_value,
        method = "Yue-Pilon prewhitened Sen slope"
      ))
    }
  }

  slope <- theil_sen(y, t)
  intercept <- median(y - slope * t, na.rm = TRUE)
  p_value <- fallback_p(y)
  list(
    slope = slope,
    intercept = intercept,
    p_value = p_value,
    method = "Theil-Sen with Mann-Kendall (no prewhitening)"
  )
}

daily <- read_csv(file.path(script_dir, "parker_discharge_daily_selected_gauge.csv"), show_col_types = FALSE) %>%
  mutate(
    Date = as.Date(Date),
    discharge_cfs = if ("discharge_cfs" %in% names(.)) {
      as.numeric(discharge_cfs)
    } else if ("mean_discharge_cfs" %in% names(.)) {
      as.numeric(mean_discharge_cfs)
    } else {
      NA_real_
    },
    discharge_cms = if ("discharge_cms" %in% names(.)) {
      as.numeric(discharge_cms)
    } else {
      discharge_cfs * CFS_TO_CMS
    },
    Year = if ("Year" %in% names(.)) as.integer(Year) else year(Date),
    Month = if ("Month" %in% names(.)) as.integer(Month) else month(Date)
  ) %>%
  filter(is.finite(discharge_cms), is.finite(Year), is.finite(Month)) %>%
  arrange(Date)

start_year <- min(daily$Year, na.rm = TRUE)
end_year <- max(daily$Year, na.rm = TRUE)

write_csv(daily, file.path(results_dir, "parker_discharge_daily_selected_gauge.csv"))

monthly <- daily %>%
  filter(Month %in% month_levels) %>%
  group_by(Year, Month) %>%
  summarise(
    mean_discharge_cms = mean(discharge_cms, na.rm = TRUE),
    mean_discharge_cfs = mean(discharge_cfs, na.rm = TRUE),
    n_days = n(),
    .groups = "drop"
  ) %>%
  mutate(
    MonthLabel = factor(Month, levels = month_levels, labels = month_labels),
    time = Year + (Month - 1) / 12
  )

month_means <- monthly %>%
  group_by(MonthLabel) %>%
  summarise(
    month_mean_cms = mean(mean_discharge_cms, na.rm = TRUE),
    month_mean_cfs = mean(mean_discharge_cfs, na.rm = TRUE),
    .groups = "drop"
  )

monthly_anom <- monthly %>%
  left_join(month_means, by = "MonthLabel") %>%
  mutate(
    discharge_anomaly_cms = mean_discharge_cms - month_mean_cms,
    discharge_anomaly_cfs = mean_discharge_cfs - month_mean_cfs
  ) %>%
  arrange(time)

write_csv(monthly_anom, file.path(results_dir, "parker_discharge_monthly_anomalies.csv"))

season_lookup <- tibble(
  Month = month_levels,
  Season = rep(1:3, each = 3),
  SeasonLabel = factor(
    rep(1:3, each = 3),
    levels = 1:3,
    labels = c("Spring (Mar-May)", "Summer (Jun-Aug)", "Fall (Sep-Nov)")
  )
)

n_season <- length(unique(season_lookup$Season))

seasonal <- daily %>%
  filter(Month %in% month_levels) %>%
  inner_join(season_lookup, by = "Month") %>%
  group_by(Year, Season, SeasonLabel) %>%
  summarise(
    mean_discharge_cms = mean(discharge_cms, na.rm = TRUE),
    mean_discharge_cfs = mean(discharge_cfs, na.rm = TRUE),
    n_days = n(),
    .groups = "drop"
  ) %>%
  mutate(time = Year + (Season - 1) / n_season)

season_means <- seasonal %>%
  group_by(Season, SeasonLabel) %>%
  summarise(
    season_mean_cms = mean(mean_discharge_cms, na.rm = TRUE),
    season_mean_cfs = mean(mean_discharge_cfs, na.rm = TRUE),
    .groups = "drop"
  )

seasonal_anom <- seasonal %>%
  left_join(season_means, by = c("Season", "SeasonLabel")) %>%
  mutate(
    discharge_anomaly_cms = mean_discharge_cms - season_mean_cms,
    discharge_anomaly_cfs = mean_discharge_cfs - season_mean_cfs
  ) %>%
  arrange(time)

write_csv(seasonal_anom, file.path(results_dir, "parker_discharge_seasonal_anomalies.csv"))

seasonal_yearly_stats <- daily %>%
  filter(Month %in% month_levels) %>%
  inner_join(season_lookup, by = "Month") %>%
  group_by(Year, Season, SeasonLabel) %>%
  summarise(
    mean_discharge_cms = mean(discharge_cms, na.rm = TRUE),
    mean_discharge_cfs = mean(discharge_cfs, na.rm = TRUE),
    sd_discharge_cms = sd(discharge_cms, na.rm = TRUE),
    var_discharge_cms2 = var(discharge_cms, na.rm = TRUE),
    cv_discharge = if_else(mean_discharge_cms > 0, sd_discharge_cms / mean_discharge_cms, NA_real_),
    n_days = n(),
    .groups = "drop"
  ) %>%
  arrange(Season, Year)

write_csv(seasonal_yearly_stats, file.path(results_dir, "parker_discharge_seasonal_yearly_stats.csv"))

seasonal_variation_trends <- seasonal_yearly_stats %>%
  group_by(Season, SeasonLabel) %>%
  nest() %>%
  mutate(
    trend_mean = map(data, ~ calc_trend(.x$mean_discharge_cms, .x$Year)),
    trend_sd = map(data, ~ calc_trend(.x$sd_discharge_cms, .x$Year)),
    trend_cv = map(data, ~ calc_trend(.x$cv_discharge, .x$Year)),
    mean_slope_cms_per_year = map_dbl(trend_mean, "slope"),
    mean_slope_cms_per_decade = mean_slope_cms_per_year * 10,
    mean_p_value = map_dbl(trend_mean, "p_value"),
    mean_method = map_chr(trend_mean, "method"),
    sd_slope_cms_per_year = map_dbl(trend_sd, "slope"),
    sd_slope_cms_per_decade = sd_slope_cms_per_year * 10,
    sd_p_value = map_dbl(trend_sd, "p_value"),
    sd_method = map_chr(trend_sd, "method"),
    cv_slope_per_year = map_dbl(trend_cv, "slope"),
    cv_slope_per_decade = cv_slope_per_year * 10,
    cv_p_value = map_dbl(trend_cv, "p_value"),
    cv_method = map_chr(trend_cv, "method"),
    n_years = map_int(data, nrow)
  ) %>%
  ungroup() %>%
  mutate(
    mean_q_value = p.adjust(mean_p_value, method = "BH"),
    sd_q_value = p.adjust(sd_p_value, method = "BH"),
    cv_q_value = p.adjust(cv_p_value, method = "BH"),
    mean_sig_q = sig_label(mean_q_value),
    sd_sig_q = sig_label(sd_q_value),
    cv_sig_q = sig_label(cv_q_value)
  ) %>%
  select(
    Season, SeasonLabel, n_years,
    mean_slope_cms_per_year, mean_slope_cms_per_decade, mean_p_value, mean_q_value, mean_sig_q, mean_method,
    sd_slope_cms_per_year, sd_slope_cms_per_decade, sd_p_value, sd_q_value, sd_sig_q, sd_method,
    cv_slope_per_year, cv_slope_per_decade, cv_p_value, cv_q_value, cv_sig_q, cv_method
  )

write_csv(seasonal_variation_trends, file.path(results_dir, "parker_discharge_seasonal_variation_trends.csv"))

trend_monthly <- calc_trend(monthly_anom$discharge_anomaly_cms, monthly_anom$time)

annual <- daily %>%
  group_by(Year) %>%
  summarise(
    mean_discharge_cms = mean(discharge_cms, na.rm = TRUE),
    mean_discharge_cfs = mean(discharge_cfs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Year)

trend_annual <- calc_trend(annual$mean_discharge_cms, annual$Year)
annual_mean_cms <- mean(annual$mean_discharge_cms, na.rm = TRUE)
annual_pct_decade <- if (is.finite(trend_annual$slope) && annual_mean_cms > 0) {
  (trend_annual$slope * 10 / annual_mean_cms) * 100
} else {
  NA_real_
}

safe_wilcox_p <- function(x, y) {
  if (length(x) < 2 || length(y) < 2) return(NA_real_)
  out <- try(wilcox.test(x, y)$p.value, silent = TRUE)
  if (inherits(out, "try-error")) return(NA_real_)
  as.numeric(out)
}

safe_t_p <- function(x, y) {
  if (length(x) < 2 || length(y) < 2) return(NA_real_)
  out <- try(t.test(x, y)$p.value, silent = TRUE)
  if (inherits(out, "try-error")) return(NA_real_)
  as.numeric(out)
}

cut_year <- 2014L
prepost_all <- monthly_anom %>%
  summarise(
    window = "All months",
    n_pre = sum(Year < cut_year),
    n_post = sum(Year >= cut_year),
    pre_mean_cms = mean(mean_discharge_cms[Year < cut_year], na.rm = TRUE),
    post_mean_cms = mean(mean_discharge_cms[Year >= cut_year], na.rm = TRUE),
    delta_post_minus_pre_cms = post_mean_cms - pre_mean_cms,
    wilcox_p_value = safe_wilcox_p(mean_discharge_cms[Year < cut_year], mean_discharge_cms[Year >= cut_year]),
    t_test_p_value = safe_t_p(mean_discharge_cms[Year < cut_year], mean_discharge_cms[Year >= cut_year])
  )

prepost_by_month <- monthly_anom %>%
  group_by(Month, MonthLabel) %>%
  summarise(
    window = as.character(first(MonthLabel)),
    n_pre = sum(Year < cut_year),
    n_post = sum(Year >= cut_year),
    pre_mean_cms = mean(mean_discharge_cms[Year < cut_year], na.rm = TRUE),
    post_mean_cms = mean(mean_discharge_cms[Year >= cut_year], na.rm = TRUE),
    delta_post_minus_pre_cms = post_mean_cms - pre_mean_cms,
    wilcox_p_value = safe_wilcox_p(mean_discharge_cms[Year < cut_year], mean_discharge_cms[Year >= cut_year]),
    t_test_p_value = safe_t_p(mean_discharge_cms[Year < cut_year], mean_discharge_cms[Year >= cut_year]),
    .groups = "drop"
  ) %>%
  select(-Month, -MonthLabel)

prepost_tbl <- bind_rows(prepost_all, prepost_by_month) %>%
  mutate(
    wilcox_q_value = p.adjust(wilcox_p_value, method = "BH"),
    t_test_q_value = p.adjust(t_test_p_value, method = "BH"),
    pre_mean_cfs = pre_mean_cms / CFS_TO_CMS,
    post_mean_cfs = post_mean_cms / CFS_TO_CMS,
    delta_post_minus_pre_cfs = delta_post_minus_pre_cms / CFS_TO_CMS
  )

write_csv(prepost_tbl, file.path(results_dir, "parker_discharge_prepost2014_tests.csv"))

trend_q_vals <- p.adjust(c(trend_monthly$p_value, trend_annual$p_value), method = "BH")

summary_tbl <- tibble(
  site_no = site_no,
  station_nm = station_nm,
  start_year = start_year,
  end_year = end_year,
  n_daily = nrow(daily),
  n_monthly = nrow(monthly_anom),
  n_seasonal = nrow(seasonal_anom),
  monthly_anomaly_slope_cms_per_year = trend_monthly$slope,
  monthly_anomaly_slope_cms_per_decade = trend_monthly$slope * 10,
  monthly_anomaly_p_value = trend_monthly$p_value,
  monthly_anomaly_q_value = trend_q_vals[[1]],
  monthly_anomaly_sig_q = sig_label(trend_q_vals[[1]]),
  monthly_anomaly_method = trend_monthly$method,
  annual_mean_slope_cms_per_year = trend_annual$slope,
  annual_mean_slope_cms_per_decade = trend_annual$slope * 10,
  annual_mean_p_value = trend_annual$p_value,
  annual_mean_q_value = trend_q_vals[[2]],
  annual_mean_sig_q = sig_label(trend_q_vals[[2]]),
  annual_mean_method = trend_annual$method,
  annual_mean_baseline_cms = annual_mean_cms,
  annual_mean_percent_change_per_decade = annual_pct_decade,
  pre2014_mean_monthly_cms = prepost_all$pre_mean_cms[[1]],
  post2014_mean_monthly_cms = prepost_all$post_mean_cms[[1]],
  post_minus_pre_monthly_cms = prepost_all$delta_post_minus_pre_cms[[1]],
  prepost2014_wilcox_p_value = prepost_all$wilcox_p_value[[1]],
  prepost2014_wilcox_q_value = prepost_tbl$wilcox_q_value[prepost_tbl$window == "All months"],
  prepost2014_t_test_p_value = prepost_all$t_test_p_value[[1]],
  prepost2014_t_test_q_value = prepost_tbl$t_test_q_value[prepost_tbl$window == "All months"]
)

write_csv(summary_tbl, file.path(results_dir, "parker_discharge_trend_summary.csv"))

p_discharge <- ggplot(monthly_anom, aes(x = time, y = discharge_anomaly_cms)) +
  geom_hline(yintercept = 0, color = "#BBBBBB", linewidth = 0.3) +
  geom_point(aes(color = MonthLabel), size = 1.8, alpha = 0.85) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x, k = 6),
    se = TRUE,
    color = pal_oi["blue"],
    fill = pal_oi["sky"],
    linewidth = 0.8
  ) +
  geom_abline(
    intercept = trend_monthly$intercept,
    slope = trend_monthly$slope,
    color = pal_oi["maroon"],
    linewidth = 0.7,
    linetype = "dashed"
  ) +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(
    title = "Parker River monthly discharge anomalies",
    subtitle = paste0(
      "USGS ", site_no, " (", station_nm, "), ", start_year, "-", end_year,
      "; March-November monthly means"
    ),
    x = "Year",
    y = expression("Discharge anomaly ("*m^3*"/s)"),
    color = "Month"
  ) +
  theme_nyt(gridline_x = TRUE, gridline_y = TRUE)

seasonal_plot_df <- seasonal_yearly_stats %>%
  select(Year, SeasonLabel, mean_discharge_cms, sd_discharge_cms) %>%
  pivot_longer(
    cols = c(mean_discharge_cms, sd_discharge_cms),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = factor(
      metric,
      levels = c("mean_discharge_cms", "sd_discharge_cms"),
      labels = c("Seasonal mean flow", "Seasonal flow variation (SD)")
    )
  )

p_seasonal <- ggplot(seasonal_plot_df, aes(x = Year, y = value, color = SeasonLabel)) +
  geom_point(size = 1.7, alpha = 0.85) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  facet_wrap(~ metric, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(
    title = "Parker River seasonal flow and variability",
    subtitle = paste0("Spring-Mar-May; Summer-Jun-Aug; Fall-Sep-Nov; ", start_year, "-", end_year),
    x = "Year",
    y = expression("Discharge ("*m^3*"/s)"),
    color = "Season"
  ) +
  theme_nyt(gridline_x = TRUE, gridline_y = TRUE)

p_supplementary <- p_discharge / p_seasonal +
  plot_annotation(
    title = "Supplementary Figure 1. Parker River discharge anomalies and seasonal flow variability",
    subtitle = paste0("USGS ", site_no, " (", station_nm, "), ", start_year, "-", end_year)
  )

save_nyt(file.path(figures_dir, "parker_discharge_anomalies_panel.pdf"), p_discharge, width = 10, height = 6)
save_nyt(file.path(figures_dir, "parker_discharge_anomalies_panel.png"), p_discharge, width = 10, height = 6, dpi = 300)
save_nyt(file.path(figures_dir, "parker_discharge_seasonal_mean_variability_panel.pdf"), p_seasonal, width = 11, height = 8)
save_nyt(file.path(figures_dir, "parker_discharge_seasonal_mean_variability_panel.png"), p_seasonal, width = 11, height = 8, dpi = 300)
save_nyt(file.path(figures_dir, "Supplementary_Figure1_parker_discharge.pdf"), p_supplementary, width = 11, height = 14)
save_nyt(file.path(figures_dir, "Supplementary_Figure1_parker_discharge.png"), p_supplementary, width = 11, height = 14, dpi = 300)

message("Done. Wrote discharge results to ", results_dir, " and figures to ", figures_dir)
