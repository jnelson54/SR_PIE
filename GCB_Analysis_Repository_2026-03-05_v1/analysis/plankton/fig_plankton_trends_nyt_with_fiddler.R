#!/usr/bin/env Rscript

# Regenerates fig_plankton_trends_nyt with mud fiddler reference band.
# Trend significance is classified using BH-FDR adjusted q-values.

suppressPackageStartupMessages({
  library(optparse)
  library(tidyverse)
  library(patchwork)
})

source("../common/theme_nyt.R")

opt <- OptionParser(option_list = list(
  make_option("--mix", type = "character", default = NA_character_,
              help = "Path to aggregated_contributions.csv"),
  make_option("--out_pdf", type = "character", default = "../../figures/fig_plankton_trends_nyt_with_fiddler.pdf"),
  make_option("--out_png", type = "character", default = "../../figures/fig_plankton_trends_nyt_with_fiddler.png")
)) |>
  parse_args()

pick_mix <- function(x) {
  candidates <- unique(na.omit(c(
    x,
    "aggregated_contributions.csv"
  )))
  hit <- candidates[file.exists(candidates)]
  if (length(hit)) hit[1] else {
    stop("Could not find aggregated_contributions.csv. Tried: ", paste(candidates, collapse = ", "))
  }
}

mix_path <- pick_mix(opt$mix)
message("[fig_plankton_trends_nyt] Using mix table: ", normalizePath(mix_path, winslash = "/", mustWork = TRUE))

df <- suppressMessages(readr::read_csv(mix_path, show_col_types = FALSE))

# Harmonize minimal columns
nm <- tolower(names(df))
names(df) <- nm
req <- c("source", "mean_contr", "region", "year")
missing <- setdiff(req, names(df))
if (length(missing)) stop("Missing columns in mix table: ", paste(missing, collapse = ", "))

# Choose species column (animal or species_code)
species_col <- if ("animal" %in% names(df)) {
  "animal"
} else if ("species_code" %in% names(df)) {
  "species_code"
} else {
  NA_character_
}
if (is.na(species_col)) stop("No species column detected (animal or species_code)")

canon_species <- function(x) {
  x <- stringr::str_to_lower(x)
  dplyr::case_when(
    stringr::str_detect(x, "blue_m|blue.*mussel") ~ "Blue mussels",
    stringr::str_detect(x, "mummi") ~ "mummichog",
    stringr::str_detect(x, "silver") ~ "silversides",
    stringr::str_detect(x, "softshell|soft shell") ~ "softshell clam",
    stringr::str_detect(x, "fiddler|mud") ~ "mud fiddler",
    TRUE ~ stringr::str_to_sentence(x)
  )
}

df1 <- df %>%
  transmute(
    species = canon_species(.data[[species_col]]),
    source = as.character(source),
    site = dplyr::coalesce(.data$site, NA_character_),
    region = as.character(region),
    year = as.integer(year),
    mean_Contr = as.numeric(mean_contr)
  ) %>%
  filter(year >= 2009) %>%
  filter(stringr::str_detect(stringr::str_to_lower(source), "pom|phyto|plankton")) %>%
  filter(species %in% c("Blue mussels", "mummichog", "silversides", "softshell clam", "mud fiddler")) %>%
  drop_na(mean_Contr)

stopifnot(nrow(df1) > 0)

df1 <- df1 %>%
  mutate(
    species = forcats::fct_drop(as.factor(species)),
    region = forcats::fct_drop(as.factor(region)),
    site = as.factor(site),
    year0 = year - min(year, na.rm = TRUE)
  )

yr_min <- min(df1$year)
yr_max <- max(df1$year)

# ---- Panel A: Region medians with BH-FDR trend classification ----
medA <- df1 %>%
  group_by(region, year) %>%
  summarize(med = median(mean_Contr, na.rm = TRUE), .groups = "drop") %>%
  mutate(year0 = year - min(year))

slopesA <- medA %>%
  group_by(region) %>%
  group_modify(~ {
    m <- lm(med ~ year0, data = .x)
    broom::tidy(m) %>% filter(term == "year0")
  }) %>%
  ungroup() %>%
  mutate(
    q.value = p.adjust(p.value, method = "BH"),
    cat = case_when(
      estimate > 0 & q.value < 0.05 ~ "significant increase",
      estimate < 0 & q.value < 0.05 ~ "significant decrease",
      TRUE ~ "no trend"
    )
  ) %>%
  select(region, slope = estimate, p.value, q.value, cat)

segA <- medA %>%
  group_by(region) %>%
  summarize(x = yr_min, xend = yr_max, .groups = "drop") %>%
  left_join(slopesA, by = "region") %>%
  group_by(region) %>%
  mutate(
    y = predict(
      lm(med ~ year0, data = medA[medA$region == first(region), ]),
      newdata = data.frame(year0 = yr_min - min(medA$year))
    ),
    yend = predict(
      lm(med ~ year0, data = medA[medA$region == first(region), ]),
      newdata = data.frame(year0 = yr_max - min(medA$year))
    )
  ) %>%
  ungroup()

sig_cols <- c(
  "significant increase" = "#E74C3C",
  "significant decrease" = "#E74C3C",
  "no trend" = "#666666"
)

pA <- ggplot() +
  geom_line(data = medA, aes(year, med, group = region), color = "grey70", linewidth = 0.4) +
  geom_point(data = medA, aes(year, med), color = "grey35", size = 1.8) +
  geom_segment(
    data = segA,
    aes(x = x, xend = xend, y = y, yend = yend, color = cat),
    linewidth = 1.4,
    lineend = "round"
  ) +
  scale_color_manual(values = sig_cols, name = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "A  %Plankton in diets (yearly medians)",
    y = "%Plankton (median across species)",
    x = NULL
  ) +
  theme_nyt(gridline_x = TRUE, gridline_y = TRUE) +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    legend.text = element_text(size = 11)
  ) +
  guides(color = guide_legend(override.aes = list(linewidth = 2)))

pA <- pA +
  geom_text(
    data = segA %>% mutate(region_label = stringr::str_to_title(as.character(region))),
    aes(x = xend + 0.3, y = yend, label = region_label),
    hjust = 0,
    size = 3.5,
    color = "#333333"
  ) +
  coord_cartesian(xlim = c(yr_min, yr_max + 1), clip = "off")

# Light green band indicating fiddler crab reference range from MixSIAR
# Mean +/- SD from 10 fiddler crabs: 50.9% +/- 4.8% (POM only)
fiddler_mean <- 0.509
fiddler_sd <- 0.048
fiddler_lo <- max(0, fiddler_mean - fiddler_sd)
fiddler_hi <- min(1, fiddler_mean + fiddler_sd)
fiddler_col <- "#1b9e77"
x_stop <- min(2024, yr_max)

pA <- pA +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = fiddler_lo, ymax = fiddler_hi,
           fill = fiddler_col, alpha = 0.25) +
  geom_segment(
    aes(x = yr_min, xend = x_stop, y = fiddler_mean, yend = fiddler_mean),
    inherit.aes = FALSE,
    linetype = "dotted",
    linewidth = 0.8,
    color = fiddler_col,
    alpha = 1
  ) +
  annotate(
    "text",
    x = 2010,
    y = fiddler_mean,
    label = "Mud Fiddler Crab\nMarine plankton (POM, MixSIAR)\nmean +/- SD",
    hjust = 0,
    vjust = 0.5,
    size = 3.2,
    color = fiddler_col,
    fontface = "italic",
    lineheight = 1.0
  )

# ---- Panel B: by-species trends with BH-FDR classification ----
segB <- df1 %>%
  filter(species != "mud fiddler") %>%
  group_by(species) %>%
  group_modify(~ {
    d <- .x
    m <- lm(mean_Contr ~ year0, data = d)
    co <- summary(m)$coefficients
    pval <- co["year0", "Pr(>|t|)"]
    slope <- co["year0", "Estimate"]
    data.frame(
      x = yr_min,
      xend = yr_max,
      y = pmin(pmax(predict(m, newdata = data.frame(year0 = 0)), 0), 1),
      yend = pmin(pmax(predict(m, newdata = data.frame(year0 = max(d$year0))), 0), 1),
      slope = slope,
      p.value = pval
    )
  }) %>%
  ungroup() %>%
  mutate(
    q.value = p.adjust(p.value, method = "BH"),
    cat = case_when(
      slope > 0 & q.value < 0.05 ~ "significant increase",
      slope < 0 & q.value < 0.05 ~ "significant decrease",
      TRUE ~ "no trend"
    )
  )

ptsB <- df1 %>%
  filter(species != "mud fiddler") %>%
  group_by(species, year) %>%
  summarize(mean = mean(mean_Contr), .groups = "drop")

pB <- ggplot() +
  geom_line(data = ptsB, aes(year, mean, group = species), color = "grey70", linewidth = 0.4) +
  geom_point(data = ptsB, aes(year, mean), color = "grey35", size = 2.2) +
  geom_segment(
    data = segB,
    aes(x = x, xend = xend, y = y, yend = yend, color = cat),
    linewidth = 1.2,
    lineend = "round"
  ) +
  facet_wrap(~ species, ncol = 2, scales = "free_y") +
  scale_color_manual(values = sig_cols, name = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "B  %Plankton by species",
    x = "Year",
    y = "%Marine plankton (POM source)"
  ) +
  theme_nyt(gridline_x = TRUE, gridline_y = TRUE) +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    strip.text = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 11)
  ) +
  guides(color = guide_legend(override.aes = list(linewidth = 2)))

fig <- pA / patchwork::plot_spacer() / pB + patchwork::plot_layout(heights = c(1, 0.08, 1.2))

ggsave(opt$out_pdf, fig, width = 12, height = 11, dpi = 300, bg = "white")
ggsave(opt$out_png, fig, width = 12, height = 11, dpi = 300, bg = "white")
message("Wrote: ", normalizePath(opt$out_pdf, winslash = "/", mustWork = FALSE))
message("Wrote: ", normalizePath(opt$out_png, winslash = "/", mustWork = FALSE))
