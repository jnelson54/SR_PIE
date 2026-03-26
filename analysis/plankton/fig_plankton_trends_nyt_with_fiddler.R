#!/usr/bin/env Rscript

install_if_missing <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    install.packages(missing, repos = "https://cloud.r-project.org")
  }
}

install_if_missing(c("optparse", "readr", "dplyr", "tidyr", "stringr", "forcats", "ggplot2", "patchwork", "scales"))

suppressPackageStartupMessages({
  library(optparse)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(forcats)
  library(ggplot2)
  library(patchwork)
  library(scales)
})

get_script_dir <- function() {
  file_arg <- "--file="
  matched <- grep(file_arg, commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(matched) == 0) {
    return(normalizePath(getwd(), winslash = "/", mustWork = TRUE))
  }
  normalizePath(dirname(sub(file_arg, "", matched[1])), winslash = "/", mustWork = TRUE)
}

repo_root <- normalizePath(file.path(get_script_dir(), "..", ".."), winslash = "/", mustWork = TRUE)
results_dir <- file.path(repo_root, "results", "plankton")
figures_dir <- file.path(repo_root, "figures")
source_data_dir <- file.path(repo_root, "source_data")
source(file.path(repo_root, "analysis", "common", "theme_nyt.R"))

if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)
if (!dir.exists(figures_dir)) dir.create(figures_dir, recursive = TRUE)
if (!dir.exists(source_data_dir)) dir.create(source_data_dir, recursive = TRUE)

opt <- OptionParser(option_list = list(
  make_option("--mix", type = "character", default = NA_character_, help = "Path to aggregated_contributions.csv"),
  make_option("--out_pdf", type = "character", default = file.path(figures_dir, "fig_plankton_trends_nyt_with_fiddler.pdf")),
  make_option("--out_png", type = "character", default = file.path(figures_dir, "fig_plankton_trends_nyt_with_fiddler.png"))
)) |>
  parse_args()

pick_mix <- function(x) {
  candidates <- unique(na.omit(c(
    x,
    file.path(repo_root, "analysis", "plankton", "aggregated_contributions.csv"),
    file.path(get_script_dir(), "aggregated_contributions.csv"),
    "aggregated_contributions.csv"
  )))
  hit <- candidates[file.exists(candidates)]
  if (length(hit)) hit[1] else stop("Could not find aggregated_contributions.csv")
}

canon_species <- function(x) {
  x <- str_to_lower(x)
  case_when(
    str_detect(x, "blue_m|blue.*mussel") ~ "Blue mussels",
    str_detect(x, "mummi") ~ "mummichog",
    str_detect(x, "silver") ~ "silversides",
    str_detect(x, "softshell|soft shell") ~ "softshell clam",
    str_detect(x, "fiddler|mud") ~ "mud fiddler",
    TRUE ~ str_to_sentence(x)
  )
}

canon_source <- function(x) {
  x <- str_to_lower(x)
  case_when(
    str_detect(x, "pom|phyto|plankton") ~ "marine plankton (POM)",
    str_detect(x, "spartina|salt water cord grass") ~ "Spartina",
    str_detect(x, "benthic") ~ "benthic microalgae",
    TRUE ~ str_to_sentence(x)
  )
}

fit_linear_trend <- function(data, y_col) {
  y <- data[[y_col]]
  x <- data$year0
  fit <- lm(y ~ x, data = data.frame(y = y, x = x))
  coef_tbl <- summary(fit)$coefficients
  tibble(
    slope = unname(coef_tbl["x", "Estimate"]),
    intercept = unname(coef_tbl["(Intercept)", "Estimate"]),
    p_value = unname(coef_tbl["x", "Pr(>|t|)"])
  )
}

fit_species_trend <- function(data) {
  fit <- lm(mean_contr ~ year0 + region, data = data)
  coef_tbl <- summary(fit)$coefficients
  tibble(
    slope = unname(coef_tbl["year0", "Estimate"]),
    intercept = unname(coef_tbl["(Intercept)", "Estimate"]),
    p_value = unname(coef_tbl["year0", "Pr(>|t|)"]),
    model = "lm(mean_contr ~ year0 + region)"
  )
}

mix_path <- pick_mix(opt$mix)
raw <- read_csv(mix_path, show_col_types = FALSE)
names(raw) <- tolower(names(raw))

species_col <- if ("animal" %in% names(raw)) {
  "animal"
} else if ("species_code" %in% names(raw)) {
  "species_code"
} else {
  NA_character_
}
if (is.na(species_col)) stop("No species column detected")

food_web <- raw %>%
  transmute(
    species = canon_species(.data[[species_col]]),
    region = str_to_lower(as.character(region)),
    source = canon_source(as.character(source)),
    year = as.integer(year),
    mean_contr = as.numeric(mean_contr)
  ) %>%
  filter(
    year >= 2009,
    source == "marine plankton (POM)",
    region %in% c("upper", "middle", "lower"),
    species %in% c("Blue mussels", "mummichog", "silversides", "softshell clam", "mud fiddler"),
    is.finite(mean_contr)
  )

native_species <- c("Blue mussels", "mummichog", "silversides", "softshell clam")

native_species_year <- food_web %>%
  filter(species %in% native_species) %>%
  group_by(species, region, year) %>%
  summarise(
    mean_contr = mean(mean_contr, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(year0 = year - min(year, na.rm = TRUE))

region_year <- native_species_year %>%
  group_by(region, year) %>%
  summarise(
    median_contr = median(mean_contr, na.rm = TRUE),
    n_species = n(),
    .groups = "drop"
  ) %>%
  mutate(year0 = year - min(year, na.rm = TRUE))

species_year <- native_species_year %>%
  group_by(species, year) %>%
  summarise(
    mean_contr = mean(mean_contr, na.rm = TRUE),
    n_region = n(),
    .groups = "drop"
  ) %>%
  mutate(year0 = year - min(year, na.rm = TRUE))

species_region_year <- native_species_year %>%
  mutate(region = factor(region, levels = c("upper", "middle", "lower")))

region_trends <- region_year %>%
  group_by(region) %>%
  group_modify(~ fit_linear_trend(.x, "median_contr")) %>%
  ungroup() %>%
  mutate(test_set = "region")

species_trends <- species_year %>%
  group_by(species) %>%
  group_modify(~ {
    species_name <- .y$species[[1]]
    fit_species_trend(filter(species_region_year, species == species_name))
  }) %>%
  ungroup() %>%
  mutate(test_set = "species")

# Plot each species fit as a single segment so the displayed trend is strictly straight.
species_plot_segments <- species_year %>%
  group_by(species) %>%
  group_modify(~ {
    fit <- lm(mean_contr ~ year0, data = .x)
    year_start <- min(.x$year, na.rm = TRUE)
    year_end <- max(.x$year, na.rm = TRUE)
    year0_start <- min(.x$year0, na.rm = TRUE)
    year0_end <- max(.x$year0, na.rm = TRUE)
    tibble(
      x = year_start,
      xend = year_end,
      y = unname(predict(fit, newdata = tibble(year0 = year0_start))),
      yend = unname(predict(fit, newdata = tibble(year0 = year0_end)))
    )
  }) %>%
  ungroup()

all_tests <- bind_rows(
  region_trends %>% transmute(id = region, test_set, slope, intercept, p_value),
  species_trends %>% transmute(id = species, test_set, slope, intercept, p_value)
) %>%
  mutate(
    q_value = p.adjust(p_value, method = "BH"),
    sig_q = case_when(
      slope > 0 & q_value < 0.05 ~ "significant increase",
      slope < 0 & q_value < 0.05 ~ "significant decrease",
      TRUE ~ "no trend"
    )
  )

region_trends <- region_trends %>%
  left_join(all_tests %>% filter(test_set == "region") %>% select(id, q_value, sig_q), by = c("region" = "id")) %>%
  arrange(factor(region, levels = c("upper", "middle", "lower")))

species_trends <- species_trends %>%
  left_join(all_tests %>% filter(test_set == "species") %>% select(id, q_value, sig_q), by = c("species" = "id")) %>%
  arrange(factor(species, levels = native_species))

fiddler_path <- file.path(repo_root, "analysis", "plankton", "fiddler_crab_contributions_filtered.csv")
fiddler_raw <- if (file.exists(fiddler_path)) read_csv(fiddler_path, show_col_types = FALSE) else tibble()

fiddler_summary <- if (nrow(fiddler_raw) > 0) {
  names(fiddler_raw) <- tolower(names(fiddler_raw))
  if (all(c("source", "mean_contr", "sd_contr") %in% names(fiddler_raw))) {
    fiddler_raw %>%
      transmute(
        source = canon_source(as.character(source)),
        mean_contr = as.numeric(mean_contr),
        sd_contr = as.numeric(sd_contr),
        n = if ("n" %in% names(fiddler_raw)) as.integer(n) else NA_integer_
      ) %>%
      filter(is.finite(mean_contr), is.finite(sd_contr)) %>%
      arrange(match(source, c("marine plankton (POM)", "Spartina", "benthic microalgae")))
  } else {
    fiddler_raw %>%
      transmute(
        source = canon_source(as.character(source)),
        source_mean_contr = as.numeric(mean_contr)
      ) %>%
      filter(is.finite(source_mean_contr)) %>%
      group_by(source) %>%
      summarise(
        mean_contr = mean(source_mean_contr, na.rm = TRUE),
        sd_contr = sd(source_mean_contr, na.rm = TRUE),
        n = n(),
        .groups = "drop"
      ) %>%
      arrange(match(source, c("marine plankton (POM)", "Spartina", "benthic microalgae")))
  }
} else {
  tibble(
    source = c("marine plankton (POM)", "Spartina", "benthic microalgae"),
    mean_contr = c(0.509, 0.478, 0.013),
    sd_contr = c(0.048, 0.050, 0.012),
    n = NA_integer_
  )
}

fiddler_pom <- fiddler_summary %>% filter(source == "marine plankton (POM)")
fiddler_mean <- fiddler_pom$mean_contr[[1]]
fiddler_sd <- fiddler_pom$sd_contr[[1]]
fiddler_lo <- max(0, fiddler_mean - fiddler_sd)
fiddler_hi <- min(1, fiddler_mean + fiddler_sd)

write_csv(region_year, file.path(results_dir, "plankton_region_year_summary.csv"))
write_csv(species_year, file.path(results_dir, "plankton_species_year_summary.csv"))
write_csv(species_region_year, file.path(results_dir, "plankton_species_region_year_summary.csv"))
write_csv(region_trends, file.path(results_dir, "plankton_region_trends.csv"))
write_csv(species_trends, file.path(results_dir, "plankton_species_trends.csv"))
write_csv(fiddler_summary, file.path(results_dir, "fiddler_mixsiar_summary.csv"))

write_csv(region_year, file.path(source_data_dir, "Figure3_plankton_region_year_summary.csv"))
write_csv(species_year, file.path(source_data_dir, "Figure3_plankton_species_year_summary.csv"))
write_csv(region_trends, file.path(source_data_dir, "Figure3_plankton_region_trends.csv"))
write_csv(species_trends, file.path(source_data_dir, "Figure3_plankton_species_trends.csv"))
write_csv(fiddler_summary, file.path(source_data_dir, "Figure3_fiddler_mixsiar_summary.csv"))

yr_min <- min(region_year$year, na.rm = TRUE)
yr_max <- max(region_year$year, na.rm = TRUE)
sig_cols <- c(
  "significant increase" = "#E74C3C",
  "significant decrease" = "#0072B2",
  "no trend" = "#666666"
)

region_segments <- region_trends %>%
  transmute(
    region,
    x = yr_min,
    xend = yr_max,
    y = intercept,
    yend = intercept + slope * (yr_max - yr_min),
    sig_q
  )

p_region <- ggplot() +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = fiddler_lo, ymax = fiddler_hi, fill = "#1b9e77", alpha = 0.18) +
  geom_segment(
    aes(x = yr_min, xend = yr_max, y = fiddler_mean, yend = fiddler_mean),
    color = "#1b9e77",
    linetype = "dotted",
    linewidth = 0.8
  ) +
  annotate(
    "text",
    x = yr_min + 0.2,
    y = min(0.98, fiddler_hi),
    label = "Mud fiddler crab\npelagic POM signal\nmean +/- SD",
    hjust = 0,
    vjust = 1,
    size = 3.2,
    color = "#1b9e77"
  ) +
  geom_line(data = region_year, aes(year, median_contr, group = region), color = "grey70", linewidth = 0.4) +
  geom_point(data = region_year, aes(year, median_contr), color = "grey35", size = 1.8) +
  geom_segment(
    data = region_segments,
    aes(x = x, xend = xend, y = y, yend = yend, color = sig_q),
    linewidth = 1.4,
    lineend = "round"
  ) +
  geom_text(
    data = region_segments %>% mutate(region_label = str_to_title(region)),
    aes(x = xend + 0.3, y = yend, label = region_label),
    color = "#333333",
    hjust = 0,
    size = 3.5
  ) +
  scale_color_manual(values = sig_cols, name = NULL) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "A  % marine plankton in diets (regional yearly medians)",
    x = NULL,
    y = "% marine plankton (POM; median across native focal species)"
  ) +
  coord_cartesian(xlim = c(yr_min, yr_max + 1), clip = "off") +
  theme_nyt(gridline_x = TRUE, gridline_y = TRUE) +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    legend.text = element_text(size = 11)
  )

species_segments <- species_trends %>%
  transmute(
    species,
    sig_q
  )

p_species <- ggplot() +
  geom_line(data = species_year, aes(year, mean_contr, group = species), color = "grey70", linewidth = 0.4) +
  geom_point(data = species_year, aes(year, mean_contr), color = "grey35", size = 2.2) +
  geom_segment(
    data = species_plot_segments %>% left_join(species_segments, by = "species"),
    aes(x = x, xend = xend, y = y, yend = yend, color = sig_q),
    linewidth = 1.2
  ) +
  facet_wrap(~ species, ncol = 2, scales = "free_y") +
  scale_color_manual(values = sig_cols, name = NULL) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "B  % marine plankton in diets of focal species",
    x = "Year",
    y = "% marine plankton (POM source)"
  ) +
  theme_nyt(gridline_x = TRUE, gridline_y = TRUE) +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    strip.text = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 11)
  )

fig <- p_region / patchwork::plot_spacer() / p_species + patchwork::plot_layout(heights = c(1, 0.08, 1.2))

ggsave(opt$out_pdf, fig, width = 12, height = 11, dpi = 300, bg = "white")
ggsave(opt$out_png, fig, width = 12, height = 11, dpi = 300, bg = "white")

message("Done. Wrote plankton results to ", results_dir, " and figures to ", figures_dir)
