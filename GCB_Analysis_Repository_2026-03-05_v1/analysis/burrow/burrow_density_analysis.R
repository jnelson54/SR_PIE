#!/usr/bin/env Rscript

# ==========================================================
# Burrow density trend (2014–present) — GLMM + robust checks
# ==========================================================

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(stringr)
  library(ggplot2); library(janitor); library(glmmTMB)
  library(splines); library(broom.mixed); library(Kendall); library(mblm);library(forcats)
})

# ------------------- paths -------------------
repo_root <- normalizePath(file.path("..", ".."), winslash = "/", mustWork = FALSE)
theme_path <- file.path(repo_root, "analysis", "common", "theme_nyt.R")
data_path  <- "burrow_density.csv"
out_dir    <- file.path(repo_root, "results", "burrow")
figs_dir   <- file.path(repo_root, "figures")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figs_dir, showWarnings = FALSE, recursive = TRUE)

# ------------------- theme -------------------
source(theme_path, local = TRUE)

# ------------------- load & standardize -------------------
raw <- readr::read_csv(data_path, show_col_types = FALSE)

bd <- raw %>%
  janitor::clean_names() %>%
  mutate(
    creek     = str_squish(str_replace_na(creek, "")),
    branch    = str_squish(str_replace_na(branch, "")),
    side      = str_squish(str_replace_na(side, "")),
    nutrient  = str_to_title(str_squish(str_replace_na(nutrient, ""))),
    habitat   = str_squish(str_replace_na(habitat, "")),
    plot_m    = `plot_location_m`,
    year      = as.integer(year),
    burrow    = burrow_density_m2
  ) %>%
  mutate(
    creek = case_when(
      str_equal(creek, "Nelson")  ~ "Nelson",
      str_equal(creek, "Nelson ") ~ "Nelson",
      TRUE ~ creek
    ),
    nutrient = if_else(nutrient %in% c("Enriched", "Enriched "), "Enriched", nutrient)
  ) %>%
  select(year, month, creek, branch, side, nutrient, habitat, plot_m, burrow) %>%
  filter(is.finite(year), is.finite(burrow)) %>%
  filter(year >= 2014) %>%
  mutate(
    year_c = year - 2014,                         # <-- centered year (PER YEAR slope)
    plot_m = suppressWarnings(as.numeric(plot_m)) # keep position numeric; drop absurd via NA
  )

stopifnot(nrow(bd) > 0)

# ------------------- quick data checks -------------------
by_year <- bd %>%
  group_by(year) %>%
  summarise(n = n(), median = median(burrow, na.rm = TRUE), .groups = "drop")

# ------------------- BOXPLOTS BY YEAR (creeks as replicates) -------------------

# 1) one value per creek-year (replicates for the boxes)
creek_year <- bd %>%
  group_by(year, creek) %>%
  summarise(med = median(burrow, na.rm = TRUE), .groups = "drop")

# 2) simple overall trend on annual across-creek medians (for the subtitle only)
annual <- creek_year %>%
  group_by(year) %>% summarise(med = median(med, na.rm = TRUE), .groups = "drop")

yr0 <- mean(annual$year, na.rm = TRUE)
fit <- lm(log1p(med) ~ I(year - yr0), data = annual)
co  <- summary(fit)$coefficients
ci  <- confint(fit)

beta  <- unname(co["I(year - yr0)","Estimate"])
ci_lo <- unname(ci["I(year - yr0)", 1])
ci_hi <- unname(ci["I(year - yr0)", 2])
pval  <- unname(co["I(year - yr0)","Pr(>|t|)"])

pct_change_year <- (exp(beta) - 1) * 100
pct_low_95      <- (exp(ci_lo) - 1) * 100
pct_high_95     <- (exp(ci_hi) - 1) * 100

# ------------------- HEATMAP: magnitude with asinh color transform -------------------

# 1) one value per creek-year = annual median density
creek_year <- bd %>%
  group_by(year, creek) %>%
  summarise(med = median(burrow, na.rm = TRUE), .groups = "drop")

# 2) order & relabel creeks by earliest non-zero (first detection)
ord <- creek_year %>%
  group_by(creek) %>%
  summarise(first_det = suppressWarnings(min(year[med > 0], na.rm = TRUE)),
            .groups = "drop") %>%
  mutate(first_det = ifelse(is.finite(first_det), first_det, Inf)) %>%
  arrange(first_det) %>%
  mutate(creek_label = paste("Creek", row_number()))

years_all <- seq(min(bd$year, na.rm = TRUE), max(bd$year, na.rm = TRUE))

heat_df <- creek_year %>%
  left_join(ord, by = "creek") %>%
  transmute(
    year_f  = factor(year, levels = years_all),
    creek_f = factor(creek_label, levels = ord$creek_label),
    med
  ) %>%
  tidyr::complete(year_f, creek_f)

# 3) asinh color transform: linear near 0, log-like at high values
knee <- 1.0  # set the knee where you want detail (m^-2). Try 0.5, 1, or 2.
asinh_trans <- scales::trans_new(
  "asinh",
  transform = function(x) asinh(x / knee),
  inverse   = function(y) knee * sinh(y)
)

# cap extreme highs so small values get more palette real-estate
cap_q <- 0.95
cap   <- max(1, as.numeric(stats::quantile(heat_df$med, cap_q, na.rm = TRUE)))
brks  <- c(0, 0.2, 0.5, 1, 2, 5, 10, 20, 40, 60, 80)
brks  <- brks[brks <= max(cap, na.rm = TRUE)]

# gentle NYT-ish sequential blues (light -> dark)
blues <- c("#F7FBFF","#E3EEF9","#D0E1F2","#AFCDE9","#8CB3DF","#6B9BD3","#4A80C3","#2A5BAD","#0B3B8C")

fig_heat <- ggplot(heat_df, aes(x = year_f, y = creek_f, fill = med)) +
  geom_tile(color = NA) +
  scale_fill_gradientn(
    colors = blues,
    trans  = asinh_trans,                 # <-- key: reveals tiny ramps
    limits = c(0, cap),                   # cap top end so late big values don't dominate
    oob    = scales::squish,
    breaks = brks, labels = brks,
    name   = expression("Burrow density (m"^-2*")")
  ) +
  labs(
    title    = "Burrow density by creek and year",
    subtitle = sprintf("Heat = annual median density per creek; color uses asinh transform (knee = %.1f m^-2) and capped at %.0f%%ile to emphasize small increases.",
                       knee, cap_q*100),
    x = "Year", y = "Creek"
  ) +
  theme_nyt(gridline_x = FALSE, gridline_y = FALSE)

ggsave(file.path(figs_dir, "fig_burrow_heatmap_asinh.png"),
       fig_heat, width = 7.4, height = 5.0, dpi = 320, bg = "white")



# ------------------- TRANSECT HURDLE HEATMAP (year × distance) -------------------

# 1) choose a bin width for distance along transect (meters)
bin_w <- 20   # try 10/20/25 depending on sampling spacing

# 2) bin distance, aggregate occupancy (any > 0) and intensity (median among > 0)
tx <- bd %>%
  filter(is.finite(plot_m)) %>%
  mutate(dist_bin = bin_w * floor(plot_m / bin_w)) %>%
  group_by(year, dist_bin) %>%
  summarise(
    n      = dplyr::n(),
    occ    = mean(burrow > 0, na.rm = TRUE),                                  # occupancy
    intens = {x <- burrow[burrow > 0]; if (length(x)) median(x, na.rm = TRUE) else NA_real_},  # intensity among present
    .groups = "drop"
  )

# 3) complete grid so missing year×bin show up (transparent tiles)
years_all <- seq(min(tx$year, na.rm = TRUE), max(tx$year, na.rm = TRUE))
bins_all  <- seq(min(tx$dist_bin, na.rm = TRUE), max(tx$dist_bin, na.rm = TRUE), by = bin_w)

tx_full <- tidyr::complete(tx,
                           year = years_all,
                           dist_bin = bins_all)

# 4) plotting: color = intensity (log1p), alpha = occupancy
#    Use a calm sequential palette; transparency encodes presence
blues <- colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))(256)

fig_tx <- ggplot(tx_full, aes(x = dist_bin, y = year)) +
  geom_tile(aes(fill = intens, alpha = occ),
            width = bin_w, height = 0.95, color = NA) +
  scale_fill_gradientn(
    colors = blues,
    trans  = scales::log1p_trans(),   # linear near 0, log-like later; zero stays in-range
    limits = c(0, NA),
    na.value = "#F2F2F2",
    name   = expression("Median density\namong occupied (m"^-2*")")
  ) +
  scale_alpha(
    range = c(0.08, 1),               # very faint when few plots have burrows
    limits = c(0, 1),
    breaks = c(0, .25, .5, .75, 1),
    labels = scales::percent,
    name   = "Occupancy"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(breaks = years_all) +
  labs(
    title = "Burrow emergence along transects through time",
    subtitle = "Color = median density among occupied plots (log1p); Transparency = occupancy (fraction of plots with any burrows)",
    x = "Distance along transect (m)",
    y = "Year"
  ) +
  theme_nyt(gridline_x = TRUE, gridline_y = TRUE) +
  guides(
    fill = guide_colorbar(barheight = grid::unit(90, "pt")),
    alpha = guide_legend(override.aes = list(fill = "#2B8CBE"))
  )

ggsave(file.path(figs_dir, "fig_transect_hurdle_heatmap.png"),
       fig_tx, width = 7.8, height = 5.2, dpi = 320, bg = "white")
