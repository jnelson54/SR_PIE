#!/usr/bin/env Rscript

install_if_missing <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    install.packages(missing, repos = "https://cloud.r-project.org")
  }
}

install_if_missing(c("readr", "dplyr", "tidyr", "stringr", "janitor", "ggplot2", "Kendall", "scales", "patchwork"))

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(janitor)
  library(ggplot2)
  library(Kendall)
  library(scales)
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
results_dir <- file.path(repo_root, "results", "burrow")
figures_dir <- file.path(repo_root, "figures")
source(file.path(repo_root, "analysis", "common", "theme_nyt.R"))

if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)
if (!dir.exists(figures_dir)) dir.create(figures_dir, recursive = TRUE)

sig_label <- function(p) {
  case_when(
    !is.finite(p) ~ NA_character_,
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    TRUE ~ "ns"
  )
}

bootstrap_ci <- function(x, stat_fn = median, conf = 0.95, n_boot = 4000) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(c(NA_real_, NA_real_))
  if (length(unique(x)) == 1) return(rep(stat_fn(x), 2))
  alpha <- (1 - conf) / 2
  draws <- replicate(n_boot, stat_fn(sample(x, length(x), replace = TRUE)))
  unname(stats::quantile(draws, probs = c(alpha, 1 - alpha), na.rm = TRUE))
}

wilson_ci <- function(successes, trials, conf = 0.95) {
  if (!is.finite(successes) || !is.finite(trials) || trials <= 0) return(c(NA_real_, NA_real_))
  z <- stats::qnorm(1 - (1 - conf) / 2)
  p_hat <- successes / trials
  denom <- 1 + (z^2 / trials)
  center <- (p_hat + (z^2 / (2 * trials))) / denom
  half_width <- (z / denom) * sqrt((p_hat * (1 - p_hat) / trials) + (z^2 / (4 * trials^2)))
  c(max(0, center - half_width), min(1, center + half_width))
}

clean_text <- function(x) {
  x %>%
    str_replace_na("") %>%
    str_squish()
}

set.seed(20260404)  # v1.1.0: fixed seed so the bootstrap intervals are identical on every run

raw <- read_csv(file.path(script_dir, "burrow_density.csv"), show_col_types = FALSE)

burrow <- raw %>%
  clean_names() %>%
  mutate(
    year = as.integer(year),
    creek = clean_text(creek),
    branch = clean_text(branch),
    side = clean_text(side),
    habitat = clean_text(habitat),
    nutrient = str_to_title(clean_text(nutrient)),
    plot_m = suppressWarnings(as.numeric(plot_location_m)),
    burrow_density_m2 = as.numeric(burrow_density_m2)
  ) %>%
  mutate(
    creek = case_when(
      creek %in% c("Nelson", "Nelson ") ~ "Nelson",
      TRUE ~ creek
    ),
    nutrient = if_else(nutrient %in% c("Enriched", "Enriched "), "Enriched", nutrient),
    transect_branch = if_else(branch != "", branch, side),
    transect_id = if_else(
      transect_branch != "",
      paste(creek, transect_branch, sep = "__"),
      creek
    )
  ) %>%
  select(year, creek, branch, side, habitat, nutrient, plot_m, transect_id, burrow_density_m2) %>%
  filter(year >= 2014, is.finite(year), is.finite(burrow_density_m2))

stopifnot(nrow(burrow) > 0)

creek_year <- burrow %>%
  group_by(year, creek) %>%
  summarise(
    median_burrow_density_m2 = median(burrow_density_m2, na.rm = TRUE),
    mean_burrow_density_m2 = mean(burrow_density_m2, na.rm = TRUE),
    n_quadrats = n(),
    .groups = "drop"
  ) %>%
  arrange(year, creek)

annual_summary <- creek_year %>%
  group_by(year) %>%
  summarise(
    across_creek_median_burrow_density_m2 = median(median_burrow_density_m2, na.rm = TRUE),
    across_creek_mean_burrow_density_m2 = mean(median_burrow_density_m2, na.rm = TRUE),
    n_creeks = n(),
    .groups = "drop"
  ) %>%
  left_join(
    creek_year %>%
      group_by(year) %>%
      summarise(
        ci_low = bootstrap_ci(median_burrow_density_m2, stat_fn = median)[1],
        ci_high = bootstrap_ci(median_burrow_density_m2, stat_fn = median)[2],
        .groups = "drop"
      ),
    by = "year"
  ) %>%
  arrange(year) %>%
  mutate(year_centered = year - mean(year, na.rm = TRUE))

transect_occupancy <- burrow %>%
  group_by(year, creek, transect_id) %>%
  summarise(
    occupied = any(burrow_density_m2 > 0, na.rm = TRUE),
    median_burrow_density_m2 = median(burrow_density_m2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year, creek, transect_id)

occupancy_summary <- transect_occupancy %>%
  group_by(year) %>%
  summarise(
    n_transects = n(),
    n_occupied = sum(occupied),
    occupancy = n_occupied / n_transects,
    occupied_median_burrow_density_m2 = median(median_burrow_density_m2[occupied], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year)

occupancy_ci <- t(mapply(wilson_ci, occupancy_summary$n_occupied, occupancy_summary$n_transects))
occupancy_summary <- occupancy_summary %>%
  mutate(
    occupancy_ci_low = occupancy_ci[, 1],
    occupancy_ci_high = occupancy_ci[, 2]
  )

fit <- lm(log1p(across_creek_median_burrow_density_m2) ~ year_centered, data = annual_summary)
coef_tbl <- summary(fit)$coefficients
ci_tbl <- confint(fit)
beta <- unname(coef_tbl["year_centered", "Estimate"])
beta_p <- unname(coef_tbl["year_centered", "Pr(>|t|)"])
beta_ci_lo <- unname(ci_tbl["year_centered", 1])
beta_ci_hi <- unname(ci_tbl["year_centered", 2])

kendall <- Kendall::MannKendall(annual_summary$across_creek_median_burrow_density_m2)
kendall_tau <- unname(kendall$tau)
kendall_p <- unname(kendall$sl)

q_vals <- p.adjust(c(beta_p, kendall_p), method = "BH")

trend_summary <- tibble(
  analysis = c("log_linear_annual_median", "kendall_annual_median"),
  response = c("across_creek_median_burrow_density_m2", "across_creek_median_burrow_density_m2"),
  estimate = c(beta, kendall_tau),
  estimate_units = c("log1p density per year", "Kendall tau"),
  p_value = c(beta_p, kendall_p),
  q_value = q_vals,
  sig_p = sig_label(p_value),
  sig_q = sig_label(q_value)
) %>%
  mutate(
    annual_percent_change = c((exp(beta) - 1) * 100, NA_real_),
    annual_percent_change_ci_low = c((exp(beta_ci_lo) - 1) * 100, NA_real_),
    annual_percent_change_ci_high = c((exp(beta_ci_hi) - 1) * 100, NA_real_),
    start_year = min(annual_summary$year, na.rm = TRUE),
    end_year = max(annual_summary$year, na.rm = TRUE),
    occupancy_2014 = occupancy_summary$occupancy[match(2014, occupancy_summary$year)],
    occupancy_2019 = occupancy_summary$occupancy[match(2019, occupancy_summary$year)],
    occupancy_latest = occupancy_summary$occupancy[which.max(occupancy_summary$year)],
    latest_year = max(occupancy_summary$year, na.rm = TRUE)
  )

supplementary_table_1 <- trend_summary %>%
  select(
    analysis, response, estimate, estimate_units,
    annual_percent_change, annual_percent_change_ci_low, annual_percent_change_ci_high,
    p_value, q_value, sig_p, sig_q,
    start_year, end_year,
    occupancy_2014, occupancy_2019, latest_year, occupancy_latest
  )

write_csv(creek_year, file.path(results_dir, "burrow_annual_creek_medians.csv"))
write_csv(annual_summary, file.path(results_dir, "burrow_annual_summary.csv"))
write_csv(transect_occupancy, file.path(results_dir, "burrow_transect_occupancy.csv"))
write_csv(occupancy_summary, file.path(results_dir, "burrow_occupancy_summary.csv"))
write_csv(trend_summary, file.path(results_dir, "burrow_trend_summary.csv"))
write_csv(supplementary_table_1, file.path(results_dir, "Supplementary_Table_4_burrow_trends.csv"))

trend_line <- tibble(
  year = annual_summary$year,
  fitted = exp(predict(fit, newdata = annual_summary)) - 1
)

p_density <- ggplot(annual_summary, aes(x = year, y = across_creek_median_burrow_density_m2)) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    color = pal_oi["blue"],
    linewidth = 0.8,
    width = 0.12
  ) +
  geom_line(color = pal_oi["blue"], linewidth = 0.8) +
  geom_point(color = pal_oi["blue"], size = 2.2) +
  geom_line(data = trend_line, aes(y = fitted), color = pal_oi["maroon"], linewidth = 0.9, linetype = "dashed") +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(
    title = "A  Mud fiddler burrow density through time",
    subtitle = paste0(
      "Across-creek annual medians with 95% CI; log-linear trend = ",
      round((exp(beta) - 1) * 100, 1), "% per year (q = ", signif(q_vals[[1]], 3), ")"
    ),
    x = "Year",
    y = expression("Burrow density ("*m^-2*")")
  ) +
  theme_nyt(gridline_x = TRUE, gridline_y = TRUE)

p_occupancy <- ggplot(occupancy_summary, aes(x = year, y = occupancy)) +
  geom_hline(yintercept = 1, color = "#BBBBBB", linewidth = 0.3) +
  # v1.1.0: the published Fig. 3b shows occupancy without interval bars. The 95% Wilson
  # intervals are still written to results/burrow/burrow_occupancy_summary.csv.
  geom_line(color = pal_oi["teal"], linewidth = 0.8) +
  geom_point(color = pal_oi["teal"], size = 2.2) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1.05)) +
  labs(
    title = "B  Mud fiddler transect occupancy through time",
    subtitle = "Occupancy is the fraction of repeated creek-branch transects with any burrows",
    x = "Year",
    y = "Occupancy"
  ) +
  theme_nyt(gridline_x = TRUE, gridline_y = TRUE)

p_burrow_panel <- p_density / p_occupancy + patchwork::plot_layout(heights = c(1, 1))

save_nyt(file.path(figures_dir, "fig_burrow_density_trend.pdf"), p_density, width = 8, height = 5)
save_nyt(file.path(figures_dir, "fig_burrow_density_trend.png"), p_density, width = 8, height = 5, dpi = 300)
save_nyt(file.path(figures_dir, "fig_burrow_occupancy_trend.pdf"), p_occupancy, width = 8, height = 5)
save_nyt(file.path(figures_dir, "fig_burrow_occupancy_trend.png"), p_occupancy, width = 8, height = 5, dpi = 300)
save_nyt(file.path(figures_dir, "fig_burrow_panel.pdf"), p_burrow_panel, width = 8, height = 9)
save_nyt(file.path(figures_dir, "fig_burrow_panel.png"), p_burrow_panel, width = 8, height = 9, dpi = 300)

message("Done. Wrote burrow results to ", results_dir, " and figures to ", figures_dir)
