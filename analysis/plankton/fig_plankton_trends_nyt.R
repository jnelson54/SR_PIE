#!/usr/bin/env Rscript

# Regenerates: fig_plankton_trends_nyt.pdf and fig_plankton_trends_nyt.png
# Inputs (auto-detected, override with --mix):
#   - data/raw/aggregated_contributions.csv OR ./aggregated_contributions.csv

suppressPackageStartupMessages({
  library(optparse)
  library(tidyverse)
  library(patchwork)
})

# Try to use GLMM if available; otherwise fall back to OLS per-species
have_glmmTMB <- requireNamespace("glmmTMB", quietly = TRUE)
have_emmeans <- requireNamespace("emmeans", quietly = TRUE)

get_script_dir <- function() {
  file_arg <- "--file="
  matched <- grep(file_arg, commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(matched) == 0) {
    return(normalizePath(getwd(), winslash = "/", mustWork = TRUE))
  }
  normalizePath(dirname(sub(file_arg, "", matched[1])), winslash = "/", mustWork = TRUE)
}

repo_root <- normalizePath(file.path(get_script_dir(), "..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "analysis", "common", "theme_nyt.R"))

opt <- OptionParser(option_list = list(
  make_option("--mix", type = "character", default = NA_character_,
              help = "Path to aggregated_contributions.csv"),
  make_option("--out_pdf", type = "character", default = file.path(repo_root, "figures", "fig_plankton_trends_nyt.pdf")),
  make_option("--out_png", type = "character", default = file.path(repo_root, "figures", "fig_plankton_trends_nyt.png"))
)) |>
  parse_args()

pick_mix <- function(x){
  candidates <- unique(na.omit(c(x,
    "data/raw/aggregated_contributions.csv",
    "aggregated_contributions.csv"
  )))
  hit <- candidates[file.exists(candidates)]
  if (length(hit)) hit[1] else stop("Could not find aggregated_contributions.csv. Tried: ", paste(candidates, collapse=", "))
}

mix_path <- pick_mix(opt$mix)
message("[fig_plankton_trends_nyt] Using mix table: ", normalizePath(mix_path, winslash = "/", mustWork = TRUE))

df <- suppressMessages(readr::read_csv(mix_path, show_col_types = FALSE))

# Harmonize minimal columns
nm <- tolower(names(df))
names(df) <- nm
req <- c("source","mean_contr","region","year")
missing <- setdiff(req, names(df))
if (length(missing)) stop("Missing columns in mix table: ", paste(missing, collapse=", "))

# Choose species column (animal or species_code), then canon names
species_col <- if ("animal" %in% names(df)) "animal" else if ("species_code" %in% names(df)) "species_code" else NA_character_
if (is.na(species_col)) stop("No species column detected (animal or species_code)")

canon_species <- function(x) {
  x <- stringr::str_to_lower(x)
  dplyr::case_when(
    stringr::str_detect(x, "blue_m|blue.*mussel") ~ "Blue mussels",
    stringr::str_detect(x, "mummi") ~ "mummichog",
    stringr::str_detect(x, "silver") ~ "silversides",
    stringr::str_detect(x, "softshell|soft shell") ~ "softshell clam",
    TRUE ~ stringr::str_to_sentence(x)
  )
}

df1 <- df %>%
  transmute(
    species = canon_species(.data[[species_col]]),
    source  = as.character(source),
    site    = dplyr::coalesce(.data$site, NA_character_),
    region  = as.character(region),
    year    = as.integer(year),
    mean_Contr = as.numeric(mean_contr)
  ) %>%
  filter(year >= 2009) %>%
  filter(stringr::str_detect(stringr::str_to_lower(source), "phyto")) %>%
  filter(species %in% c("Blue mussels","mummichog","silversides","softshell clam")) %>%
  drop_na(mean_Contr)

stopifnot(nrow(df1) > 0)

df1 <- df1 %>% mutate(
  species = forcats::fct_drop(as.factor(species)),
  region  = forcats::fct_drop(as.factor(region)),
  site    = as.factor(site),
  year0   = year - min(year, na.rm = TRUE)
)

yr_min <- min(df1$year); yr_max <- max(df1$year)

# ---- Panel A: Region medians with OLS trend classification ----
medA <- df1 %>%
  group_by(region, year) %>%
  summarize(med = median(mean_Contr, na.rm = TRUE), .groups = "drop") %>%
  mutate(year0 = year - min(year))

slopesA <- medA %>% group_by(region) %>%
  group_modify(~{
    m <- lm(med ~ year0, data = .x)
    broom::tidy(m) %>% filter(term == "year0")
  }) %>% ungroup() %>%
  mutate(cat = case_when(estimate > 0 & p.value < 0.05 ~ "↑ significant increase",
                         estimate < 0 & p.value < 0.05 ~ "↓ significant decrease",
                         TRUE ~ "no trend")) %>%
  select(region, slope = estimate, p.value, cat)

segA <- medA %>% group_by(region) %>% summarize(x=yr_min, xend=yr_max, .groups="drop") %>%
  left_join(slopesA, by="region") %>% group_by(region) %>%
  mutate(y    = predict(lm(med ~ year0, data = medA[medA$region==first(region), ]),
                        newdata = data.frame(year0 = yr_min - min(medA$year))),
         yend = predict(lm(med ~ year0, data = medA[medA$region==first(region), ]),
                        newdata = data.frame(year0 = yr_max - min(medA$year)))) %>% ungroup()

sig_cols <- c("↑ significant increase" = "#D55E00",
              "↓ significant decrease" = "#0072B2",
              "no trend"               = "#666666")

pA <- ggplot() +
  geom_line(data = medA, aes(year, med, group = region), color = "grey70", linewidth = 0.4) +
  geom_point(data = medA, aes(year, med), color = "grey35", size = 1.8) +
  geom_segment(data = segA,
               aes(x = x, xend = xend, y = y, yend = yend, color = cat),
               linewidth = 1.4, lineend = "round") +
  scale_color_manual(values = sig_cols, name = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "A  %Plankton in diets (yearly medians)",
       y = "%Plankton (median across species)", x = NULL) +
  theme_nyt(gridline_x = TRUE, gridline_y = TRUE) +
  guides(color = guide_legend(override.aes = list(linewidth = 2)))

pA <- pA +
  geom_text(data = segA %>% mutate(region_label = stringr::str_to_title(as.character(region))),
            aes(x = xend + 0.3, y = yend, label = region_label),
            hjust = 0, size = 3.5, color = "#333333") +
  coord_cartesian(xlim = c(yr_min, yr_max + 1), clip = "off")

# Light green band indicating the canonical fiddler MixSIAR reference range
pom_mean <- 0.7336946749534305; pom_sd <- 0.024599556429858803
pom_lo   <- max(0, pom_mean - pom_sd); pom_hi <- min(1, pom_mean + pom_sd)
pom_col  <- "#1b9e77"; x_stop <- min(2020, yr_max)
pA <- pA +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = pom_lo, ymax = pom_hi,
           fill = pom_col, alpha = 0.08) +
  geom_segment(aes(x = yr_min, xend = x_stop, y = pom_mean, yend = pom_mean),
               inherit.aes = FALSE, linetype = "dotted", linewidth = 0.6,
               color = pom_col, alpha = 0.9) +
  annotate("text", x = yr_min + 0.2, y = min(0.98, pom_hi - 0.012),
           label = "Fiddler Plankton Contribution ± SD",
           hjust = 0, vjust = 1, size = 3.2, color = pom_col)

# ---- Panel B: by-species segments (GLMM if available; else OLS) ----
if (have_glmmTMB && have_emmeans) {
  suppressPackageStartupMessages({ library(glmmTMB); library(emmeans) })
  # squeeze to (0,1)
  n <- nrow(df1)
  df1 <- df1 %>% mutate(y = ((mean_Contr * (n - 1)) + 0.5) / n)
  m_int <- glmmTMB::glmmTMB(y ~ year0 * species + (1|region) + (1|site),
                            family = glmmTMB::beta_family(link = "logit"), data = df1)
  sl_tab <- as.data.frame(emmeans::test(emmeans::emtrends(m_int, ~ species, var = "year0", type = "link")))
  slB <- sl_tab %>% mutate(cat = case_when(`year0.trend` > 0 & p.value < 0.05 ~ "↑ significant increase",
                                           `year0.trend` < 0 & p.value < 0.05 ~ "↓ significant decrease",
                                           TRUE ~ "no trend")) %>% select(species, cat)
  pred_end <- as.data.frame(emmeans::emmeans(m_int, ~ species | year0,
                                             at = list(year0 = c(0, max(df1$year0))),
                                             type = "response")) %>%
    mutate(year0 = as.numeric(as.character(year0)), year = year0 + yr_min) %>%
    arrange(species, year)
  segB <- pred_end %>% group_by(species) %>% summarize(x=first(year), xend=last(year),
              y=first(response), yend=last(response), .groups = "drop") %>% left_join(slB, by="species")
} else {
  # Fallback: per-species OLS on proportions (bounded later)
  segB <- df1 %>% group_by(species) %>% group_modify(~{
    d <- .x; m <- lm(mean_Contr ~ year0, data = d); co <- summary(m)$coefficients
    pval <- co["year0","Pr(>|t|)"]; slope <- co["year0","Estimate"]
    data.frame(x = yr_min, xend = yr_max,
               y = pmin(pmax(predict(m, newdata = data.frame(year0 = 0)), 0), 1),
               yend = pmin(pmax(predict(m, newdata = data.frame(year0 = max(d$year0))), 0), 1),
               cat = ifelse(slope>0 & pval<0.05, "↑ significant increase",
                            ifelse(slope<0 & pval<0.05, "↓ significant decrease", "no trend")))
  }) %>% ungroup()
}

ptsB <- df1 %>% group_by(species, year) %>% summarize(mean = mean(mean_Contr), .groups = "drop")

pB <- ggplot() +
  geom_line(data = ptsB, aes(year, mean, group = species), color = "grey70", linewidth = 0.4) +
  geom_point(data = ptsB, aes(year, mean), color = "grey35", size = 1.8) +
  geom_segment(data = segB, aes(x = x, xend = xend, y = y, yend = yend, color = cat),
               linewidth = 1.4, lineend = "round") +
  facet_wrap(~ species, ncol = 2) +
  scale_color_manual(values = sig_cols, name = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "B  %Plankton by species (trend segment)", x = "Year", y = "%Plankton (phytoplankton source)") +
  theme_nyt(gridline_x = TRUE, gridline_y = TRUE) +
  guides(color = guide_legend(override.aes = list(linewidth = 2)))

fig <- pA / patchwork::plot_spacer() / pB + patchwork::plot_layout(heights = c(1, 0.10, 1.2))

ggsave(opt$out_pdf, fig, width = 7.4, height = 6.8, dpi = 300, bg = "white")
ggsave(opt$out_png, fig, width = 7.4, height = 6.8, dpi = 300, bg = "white")
message("Wrote: ", normalizePath(opt$out_pdf, winslash = "/", mustWork = FALSE))
message("Wrote: ", normalizePath(opt$out_png, winslash = "/", mustWork = FALSE))

