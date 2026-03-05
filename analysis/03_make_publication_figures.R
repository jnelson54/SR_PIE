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
  "dplyr", "readr", "ggplot2", "scales", "stringr", "tidyr"
))

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(scales)
  library(stringr)
  library(tidyr)
})

root <- get_project_root()
in_series <- file.path(root, "analysis", "output", "sonde", "sonde_seasonal_anomaly_series.csv")
in_trends <- file.path(root, "analysis", "output", "sonde", "sonde_trend_results.csv")
fig_dir <- file.path(root, "analysis", "figures")
out_dir <- file.path(root, "analysis", "output", "sonde")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(in_series)) {
  stop("Anomaly series not found: ", in_series, "\nRun analysis/02_fit_trends_and_anomalies.R first.")
}
if (!file.exists(in_trends)) {
  stop("Trend results not found: ", in_trends, "\nRun analysis/02_fit_trends_and_anomalies.R first.")
}

series <- readr::read_csv(in_series, show_col_types = FALSE) %>%
  mutate(
    season_bin = factor(season_bin, levels = c("Jun-Jul", "Aug-Sep"), ordered = TRUE),
    combined_site = factor(
      combined_site,
      levels = c("LOWER_MAIN", "MIDDLE_MAIN"),
      labels = c("Lower Estuary", "Middle Estuary")
    )
  )

invisible(readr::read_csv(in_trends, show_col_types = FALSE))

theme_nc <- function() {
  theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.text = element_text(face = "bold", size = 10),
      axis.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10),
      legend.position = "top",
      legend.title = element_blank(),
      plot.title.position = "plot"
    )
}

season_palette <- c(
  "Jun-Jul" = "#1f78b4",
  "Aug-Sep" = "#d95f02"
)

plot_variable <- function(var_name, y_raw, y_anom, title_text, out_stub) {
  var_df <- series %>%
    filter(variable == var_name) %>%
    transmute(
      combined_site,
      season_bin,
      year,
      raw_value,
      anomaly
    )

  plot_df <- bind_rows(
    var_df %>% transmute(combined_site, season_bin, year, value = raw_value, panel = "Raw"),
    var_df %>% transmute(combined_site, season_bin, year, value = anomaly, panel = "Anomaly")
  ) %>%
    mutate(
      panel = factor(panel, levels = c("Raw", "Anomaly"), ordered = TRUE),
      panel_pretty = factor(
        ifelse(panel == "Raw", y_raw, y_anom),
        levels = c(y_raw, y_anom),
        ordered = TRUE
      )
    )

  zero_line_df <- tibble(panel_pretty = y_anom)
  p <- ggplot(plot_df, aes(x = year, y = value, color = season_bin, group = season_bin)) +
    geom_hline(
      data = zero_line_df,
      aes(yintercept = 0),
      inherit.aes = FALSE,
      linewidth = 0.4,
      color = "gray35"
    ) +
    geom_line(linewidth = 0.7, alpha = 0.8) +
    geom_point(size = 1.8, alpha = 0.9) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.8, linetype = "22") +
    facet_grid(panel_pretty ~ combined_site, scales = "free_y") +
    scale_color_manual(values = season_palette, drop = FALSE) +
    scale_x_continuous(breaks = pretty_breaks(n = 7)) +
    labs(
      x = "Year",
      y = NULL,
      title = title_text,
      subtitle = "Season bins: June-July and August-September. Trend lines are OLS fits.",
      caption = "Trend statistics: analysis/output/sonde/sonde_trend_results.csv"
    ) +
    theme_nc()

  png_file <- file.path(fig_dir, paste0(out_stub, ".png"))
  pdf_file <- file.path(fig_dir, paste0(out_stub, ".pdf"))
  ggsave(png_file, plot = p, width = 11, height = 7, dpi = 320)
  ggsave(pdf_file, plot = p, width = 11, height = 7, dpi = 320)
}

plot_variable(
  var_name = "temperature",
  y_raw = "Raw Temperature (deg C)",
  y_anom = "Temperature Anomaly (deg C)",
  title_text = "PIE Sonde Temperature: Raw Seasonal Means and Anomalies",
  out_stub = "figure_temperature_raw_anomaly"
)

plot_variable(
  var_name = "salinity",
  y_raw = "Raw Salinity (PSU)",
  y_anom = "Salinity Anomaly (PSU)",
  title_text = "PIE Sonde Salinity: Raw Seasonal Means and Anomalies",
  out_stub = "figure_salinity_raw_anomaly"
)

figure_manifest <- tibble::tibble(
  file = c(
    "figure_temperature_raw_anomaly.png",
    "figure_temperature_raw_anomaly.pdf",
    "figure_salinity_raw_anomaly.png",
    "figure_salinity_raw_anomaly.pdf"
  ),
  path = file.path("analysis", "figures", file)
)

write_csv(figure_manifest, file.path(out_dir, "figure_manifest.csv"))
message("Wrote figures to: ", fig_dir)
