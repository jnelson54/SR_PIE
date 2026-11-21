# Analyze temperature and conductivity anomalies for Plum Island LTER sonde data
# Outputs:
#   - results/pie_trend_summary.csv
#   - results/pie_early_late_trend_summary.csv
#   - results/pie_temp16_threshold_summary.csv
#   - results/pie_temp16_threshold_annual.csv
#   - figures/pie_anomalies_panel.pdf

install_if_missing <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    install.packages(missing, repos = "https://cloud.r-project.org")
  }
}

pkgs <- c("tidyverse", "mgcv", "scales", "systemfonts", "Kendall", "zyp", "lubridate")
install_if_missing(pkgs)

suppressPackageStartupMessages({
  library(tidyverse)
  library(mgcv)
  library(scales)
  library(lubridate)
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
results_dir <- file.path(repo_root, "results")
figures_dir <- file.path(repo_root, "figures")
source_data_dir <- file.path(repo_root, "source_data")
source(file.path(repo_root, "analysis", "common", "theme_nyt.R"))

if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)
if (!dir.exists(figures_dir)) dir.create(figures_dir, recursive = TRUE)
if (!dir.exists(source_data_dir)) dir.create(source_data_dir, recursive = TRUE)

# ---- Load and prepare anomaly data ----
raw <- read_csv(file.path(script_dir, "pie_temp_sal_sonde.csv"), show_col_types = FALSE)

season_levels <- sort(unique(raw$Season))
season_labels <- paste0("Season ", season_levels)
n_season <- length(season_levels)

df <- raw %>%
  mutate(
    Season = factor(Season, levels = season_levels, labels = season_labels),
    season_num = as.integer(Season),
    time = Year + (season_num - 1) / n_season,
    Region = factor(Region),
    Site = factor(Site)
  ) %>%
  arrange(Region, time)

var_labels <- c(
  Temp = "Temperature anomaly",
  Cond = "Conductivity anomaly"
)

long <- df %>%
  pivot_longer(cols = c(Temp, Cond), names_to = "variable", values_to = "value") %>%
  mutate(variable_label = var_labels[variable])

# ---- Early/Late windows for all sonde variables ----
window_long <- long %>%
  mutate(
    season_window = if_else(season_num == min(season_num, na.rm = TRUE), "Early", "Late"),
    season_window = factor(season_window, levels = c("Early", "Late"))
  )

# ---- Helpers ----
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
    p_candidates <- c(
      suppressWarnings(as.numeric(z_obj$p.value)),
      suppressWarnings(as.numeric(z_obj$p_value)),
      suppressWarnings(as.numeric(z_obj$pval)),
      suppressWarnings(as.numeric(z_obj$sig)),
      suppressWarnings(as.numeric(z_obj$sl))
    )
    p_candidates <- p_candidates[is.finite(p_candidates)]
    if (length(p_candidates) > 0) return(p_candidates[[1]])
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
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      method = "Insufficient data"
    ))
  }

  if (requireNamespace("zyp", quietly = TRUE)) {
    z <- try(zyp::zyp.trend.vector(y, t, method = "yuepilon"), silent = TRUE)
    if (!inherits(z, "try-error")) {
      slope <- if (is.list(z)) suppressWarnings(as.numeric(z$trend)) else extract_named_num(z, c("trend", "slope"))
      p_val <- extract_p_from_zyp(z)
      # zyp returns lbound/ubound as 95% CI for the Sen slope
      ci_lower <- extract_named_num(z, "lbound")
      ci_upper <- extract_named_num(z, "ubound")

      if (!is.finite(slope)) slope <- theil_sen(y, t)
      # Use robust intercept for plotting compatibility with Sen slope.
      intercept <- median(y - slope * t, na.rm = TRUE)
      if (!is.finite(p_val)) p_val <- fallback_p(y)

      return(list(
        slope = slope,
        intercept = intercept,
        p_value = p_val,
        ci_lower = ci_lower,
        ci_upper = ci_upper,
        method = "Yue-Pilon prewhitened Sen slope"
      ))
    }
  }

  slope <- theil_sen(y, t)
  intercept <- median(y - slope * t, na.rm = TRUE)
  p_val <- fallback_p(y)
  list(
    slope = slope,
    intercept = intercept,
    p_value = p_val,
    ci_lower = NA_real_,
    ci_upper = NA_real_,
    method = "Theil-Sen with Mann-Kendall (no prewhitening)"
  )
}

sig_label <- function(p) {
  dplyr::case_when(
    !is.finite(p) ~ NA_character_,
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    TRUE ~ "ns"
  )
}

# ---- Full-trend anomaly summary (no breakpoint analysis) ----
by_group <- long %>% group_by(Region, variable) %>% nest()

trend_tbl <- by_group %>%
  mutate(
    trend = map(data, ~ calc_trend(.x$value, .x$time)),
    slope = map_dbl(trend, "slope"),
    intercept = map_dbl(trend, "intercept"),
    p_value = map_dbl(trend, "p_value"),
    ci_lower = map_dbl(trend, "ci_lower"),
    ci_upper = map_dbl(trend, "ci_upper"),
    method = map_chr(trend, "method"),
    slope_decade = slope * 10,
    ci_lower_decade = ci_lower * 10,
    ci_upper_decade = ci_upper * 10,
    n = map_int(data, nrow),
    start_year = map_dbl(data, ~ min(.x$Year, na.rm = TRUE)),
    end_year = map_dbl(data, ~ max(.x$Year, na.rm = TRUE))
  ) %>%
  select(-data, -trend) %>%
  ungroup() %>%  # v1.1.0: adjust across the four region-by-variable tests (was applied within each group, so q equalled p)
  mutate(
    q_value = p.adjust(p_value, method = "BH"),
    sig_p = sig_label(p_value),
    sig_q = sig_label(q_value),
    variable_label = var_labels[variable]
  ) %>%
  relocate(variable_label, .after = variable)

write_csv(trend_tbl, file.path(results_dir, "pie_trend_summary.csv"))

# ---- Early/late trend summary (temperature + conductivity) ----
window_trend_tbl <- window_long %>%
  group_by(Region, variable, variable_label, season_window) %>%
  nest() %>%
  mutate(
    trend = map(data, ~ calc_trend(.x$value, .x$time)),
    slope = map_dbl(trend, "slope"),
    intercept = map_dbl(trend, "intercept"),
    p_value = map_dbl(trend, "p_value"),
    method = map_chr(trend, "method"),
    slope_decade = slope * 10,
    n = map_int(data, nrow),
    start_year = map_dbl(data, ~ min(.x$Year, na.rm = TRUE)),
    end_year = map_dbl(data, ~ max(.x$Year, na.rm = TRUE)),
    pre2014_mean_anomaly = map_dbl(data, ~ mean(.x$value[.x$Year < 2014], na.rm = TRUE)),
    post2014_mean_anomaly = map_dbl(data, ~ mean(.x$value[.x$Year >= 2014], na.rm = TRUE)),
    post_minus_pre_anomaly = post2014_mean_anomaly - pre2014_mean_anomaly
  ) %>%
  ungroup() %>%
  select(-data, -trend) %>%
  mutate(
    q_value = p.adjust(p_value, method = "BH"),
    sig_p = sig_label(p_value),
    sig_q = sig_label(q_value)
  ) %>%
  relocate(variable, variable_label, season_window, .after = Region)

write_csv(window_trend_tbl, file.path(results_dir, "pie_early_late_trend_summary.csv"))
# Backward-compatible filename for downstream references.
write_csv(
  window_trend_tbl %>% filter(variable == "Temp"),
  file.path(results_dir, "pie_temp_early_late_trend_summary.csv")
)

# ---- 16 C thermal-threshold analysis using high-tide temperatures ----
ht_raw <- read_csv(file.path(script_dir, "High Tide Values.csv"), show_col_types = FALSE, name_repair = "unique_quiet")

lower_date_col <- names(ht_raw)[2]
lower_temp_col <- names(ht_raw)[3]
upper_date_col <- names(ht_raw)[7]   # MB (Middle Bridge) = upper estuary
upper_temp_col <- names(ht_raw)[8]

thermal_long <- bind_rows(
  ht_raw %>%
    transmute(
      Region = "Lower",
      datetime_raw = .data[[lower_date_col]],
      temp_c = as.numeric(.data[[lower_temp_col]])
    ),
  ht_raw %>%
    transmute(
      Region = "Upper",
      datetime_raw = .data[[upper_date_col]],
      temp_c = as.numeric(.data[[upper_temp_col]])
    )
) %>%
  mutate(
    datetime = parse_date_time(
      datetime_raw,
      orders = c("m/d/y I:M p", "m/d/Y I:M p", "m/d/Y", "m/d/y"),
      tz = "America/New_York",
      quiet = TRUE
    ),
    Year = year(datetime),
    Month = month(datetime)
  ) %>%
  filter(
    is.finite(temp_c),
    !is.na(Year),
    Month %in% 3:11,
    Year >= min(df$Year, na.rm = TRUE),
    Year <= max(df$Year, na.rm = TRUE)
  )

thermal_annual <- thermal_long %>%
  group_by(Region, Year) %>%
  summarise(
    n_obs = n(),
    n_ge16 = sum(temp_c >= 16, na.rm = TRUE),
    prop_ge16 = n_ge16 / n_obs,
    mean_temp_c = mean(temp_c, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Region, Year)

thermal_trend <- thermal_annual %>%
  group_by(Region) %>%
  nest() %>%
  mutate(
    trend = map(data, ~ calc_trend(.x$prop_ge16, .x$Year)),
    slope_prop_per_year = map_dbl(trend, "slope"),
    slope_prop_per_decade = slope_prop_per_year * 10,
    slope_pct_points_per_decade = slope_prop_per_decade * 100,
    p_value = map_dbl(trend, "p_value"),
    method = map_chr(trend, "method"),
    pre2014_mean_prop_ge16 = map_dbl(data, ~ mean(.x$prop_ge16[.x$Year < 2014], na.rm = TRUE)),
    post2014_mean_prop_ge16 = map_dbl(data, ~ mean(.x$prop_ge16[.x$Year >= 2014], na.rm = TRUE)),
    delta_prop_post_minus_pre = post2014_mean_prop_ge16 - pre2014_mean_prop_ge16,
    pre2014_mean_temp_c = map_dbl(data, ~ mean(.x$mean_temp_c[.x$Year < 2014], na.rm = TRUE)),
    post2014_mean_temp_c = map_dbl(data, ~ mean(.x$mean_temp_c[.x$Year >= 2014], na.rm = TRUE)),
    delta_temp_post_minus_pre_c = post2014_mean_temp_c - pre2014_mean_temp_c,
    wilcox_p_prepost_prop = map_dbl(
      data,
      ~ {
        x <- .x$prop_ge16[.x$Year < 2014]
        y <- .x$prop_ge16[.x$Year >= 2014]
        if (length(x) < 2 || length(y) < 2) return(NA_real_)
        suppressWarnings(as.numeric(wilcox.test(x, y)$p.value))
      }
    )
  ) %>%
  select(-data, -trend) %>%
  ungroup() %>%
  mutate(
    q_value = p.adjust(p_value, method = "BH"),
    sig_p = sig_label(p_value),
    sig_q = sig_label(q_value),
    critical_temperature_c = 16
  )

write_csv(thermal_annual, file.path(results_dir, "pie_temp16_threshold_annual.csv"))
write_csv(thermal_trend, file.path(results_dir, "pie_temp16_threshold_summary.csv"))

# ---- Annual growing-season means (for figure) ----
annual_long <- long %>%
  group_by(Year, Region, variable, variable_label) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

# Refit intercept to annual means using the existing Sen slope
plot_trend_lines <- trend_tbl %>%
  select(Region, variable, variable_label, slope) %>%
  left_join(annual_long, by = c("Region", "variable", "variable_label")) %>%
  group_by(Region, variable, variable_label, slope) %>%
  summarise(intercept = median(value - slope * Year, na.rm = TRUE), .groups = "drop")

# ---- Plotting ----
p_all <- ggplot() +
  geom_hline(yintercept = 0, color = "#BBBBBB", linewidth = 0.3) +
  geom_vline(xintercept = 2014, color = pal_oi["maroon"], linewidth = 0.7, linetype = "dotted") +
  geom_point(
    data = annual_long,
    aes(x = Year, y = value),
    size = 1.8,
    color = unname(pal_oi["teal"]),
    alpha = 0.85
  ) +
  geom_smooth(
    data = annual_long,
    aes(x = Year, y = value),
    method = "gam",
    formula = y ~ s(x, k = 6),
    se = TRUE,
    color = unname(pal_oi["teal"]),
    fill = unname(pal_oi["teal"]),
    linewidth = 0.8
  ) +
  geom_abline(
    data = plot_trend_lines,
    aes(intercept = intercept, slope = slope),
    color = unname(pal_oi["blue"]),
    linewidth = 0.7,
    linetype = "dashed"
  ) +
  facet_grid(variable_label ~ Region, scales = "free_y") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  labs(
    title = "Plum Island LTER: temperature and conductivity anomalies",
    subtitle = "Growing season (May\u2013Oct); dotted line marks 2014 (first year mud fiddler crabs observed in PIE)",
    x = "Year",
    y = "Anomaly"
  ) +
  theme_nyt(gridline_x = TRUE)

save_nyt(file.path(figures_dir, "pie_anomalies_panel.pdf"), p_all, width = 11, height = 7)
save_nyt(file.path(figures_dir, "pie_anomalies_panel.png"), p_all, width = 11, height = 7, dpi = 300)

# ---- Source data exports for Figure 1 ----
# Annual growing-season mean anomalies (points in figure)
write_csv(
  annual_long %>% select(Year, Region, variable, variable_label, value),
  file.path(source_data_dir, "Figure1_hydrography_anomalies.csv")
)
# Trend summary with 95% CIs (per decade) for manuscript reporting
write_csv(
  trend_tbl %>%
    select(Region, variable, variable_label,
           slope_decade, ci_lower_decade, ci_upper_decade,
           p_value, q_value, sig_q, n, start_year, end_year, method),
  file.path(source_data_dir, "Figure1_hydrography_trends.csv")
)

message(
  "Done. Wrote results and figures to:",
  " ", results_dir, " and ", figures_dir
)
