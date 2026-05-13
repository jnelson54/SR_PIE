#!/usr/bin/env Rscript
# Does increased marine influence elevate plankton in diets?
# Panel A: region-year medians with 2014 segmented fits
# Panel B: all species together (Mummichog, Silversides, Softshell clam),
#          plotted on a common year window (falls back gracefully), with segmented fits.
# Prints significance for the 2014 step. Uses NYT theme if available.

suppressPackageStartupMessages({
  library(optparse)
  library(readr); library(dplyr); library(tidyr); library(stringr); library(janitor); library(tibble)
  library(ggplot2); library(patchwork)
  if (requireNamespace("lme4", quietly=TRUE))     library(lme4)
  if (requireNamespace("lmerTest", quietly=TRUE)) library(lmerTest)
  if (requireNamespace("conflicted", quietly=TRUE)) {
    library(conflicted)
    conflicts_prefer(dplyr::filter); conflicts_prefer(dplyr::lag)
    conflicts_prefer(lmerTest::lmer); conflicts_prefer(base::`%*%`)
  }
})

`%>%` <- dplyr::`%>%`; filter <- dplyr::filter; mutate <- dplyr::mutate
group_by <- dplyr::group_by; summarise <- dplyr::summarise; left_join <- dplyr::left_join
arrange <- dplyr::arrange

# ---------------- CLI ----------------
option_list <- list(
  make_option("--mix",    type="character", default="aggregated_contributions.csv"),
  make_option("--annual", type="character", default="data/derived/drivers_metrics_annual.csv"),
  make_option("--season", type="character", default="data/derived/drivers_metrics_season.csv"),
  make_option("--byear",  type="integer",   default=2014),
  make_option("--theme",  type="character", default="pipeline/theme_nyt.R"),
  make_option("--out",    type="character", default="figs/fig_plankton_by_species_nyt.png")
)
opt <- parse_args(OptionParser(option_list = option_list))
mix_path    <- opt$mix
annual_path <- opt$annual
season_path <- opt$season
break_year  <- as.integer(opt$byear)
theme_path  <- opt$theme
out_png     <- opt$out
dir.create(dirname(out_png), recursive = TRUE, showWarnings = FALSE)

msg <- function(...) cat(paste0(..., "\n"))
msg("Mix:    ", normalizePath(mix_path, winslash="/", mustWork=FALSE))
msg("Annual: ", normalizePath(annual_path, winslash="/", mustWork=FALSE))
msg("Season: ", normalizePath(season_path, winslash="/", mustWork=FALSE))
msg("Theme:  ", normalizePath(theme_path, winslash="/", mustWork=FALSE))
msg("Out:    ", normalizePath(out_png, winslash="/", mustWork=FALSE))

# ---------------- Theme + palettes ----------------
nyt <- theme_classic(base_size = 12)  # fallback
pal_region  <- c(lower = "#0072B2", middle = "#009E73")                 # blue / teal
pal_species <- c("Mummichog"="#0072B2","Silversides"="#E69F00","Softshell clam"="#009E73")

use_theme_file <- function(path) {
  if (!is.character(path) || is.na(path)) return(FALSE)
  if (!file.exists(path)) return(FALSE)
  source(path, local = .GlobalEnv)
  TRUE
}
if (use_theme_file(theme_path) || use_theme_file("pipeline/theme_nyt.R")) {
  if (exists("theme_nyt")) {
    nyt <- theme_nyt(gridline_x = FALSE, gridline_y = TRUE, base_size = 12)
  }
  if (exists("pal_oi")) {
    pal_region  <- c(lower = pal_oi[["blue"]],  middle = pal_oi[["teal"]])
    pal_species <- c("Mummichog"     = pal_oi[["blue"]],
                     "Silversides"    = pal_oi[["orange"]],
                     "Softshell clam" = pal_oi[["teal"]])
  }
}
theme_set(nyt)
msg("pal_region names: ", paste(names(pal_region), collapse=", "))
msg("pal_species names: ", paste(names(pal_species), collapse=", "))

# --- safe saver: pick device by extension (avoids blank PNGs from PDF devices) ---
ggsave_smart <- function(filename, plot, width, height, dpi = 320, bg = "white") {
  ext <- tolower(tools::file_ext(filename))
  dev <- switch(ext,
                "png"  = if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png",
                "pdf"  = grDevices::cairo_pdf,
                "svg"  = "svg",
                "jpg"  = "jpeg",
                "jpeg" = "jpeg",
                "tif"  = "tiff",
                "tiff" = "tiff",
                NULL   # let ggsave infer
  )
  ggplot2::ggsave(filename, plot, width = width, height = height, dpi = dpi, bg = bg, device = dev)
}

# ---------------- Helpers ----------------
pick_first <- function(nms, cand) cand[cand %in% nms][1]
species_map <- function(x){
  xl <- tolower(x)
  dplyr::case_when(
    stringr::str_detect(xl,"mummichog|fundulus") ~ "Mummichog",
    stringr::str_detect(xl,"silverside|menidia") ~ "Silversides",
    stringr::str_detect(xl,"softshell|mya")      ~ "Softshell clam",
    TRUE ~ NA_character_
  )
}
standardize_mix <- function(df){
  df <- janitor::clean_names(df)
  sp  <- pick_first(names(df), c("animal","species","species_code","consumer"))
  src <- pick_first(names(df), c("source","Source"))
  reg <- pick_first(names(df), c("region","segment","zone","region_lab"))
  yr  <- pick_first(names(df), c("year","yr","sample_year"))
  val <- pick_first(names(df), c("mean_contr","mean_percent","percent","mean","value"))
  if (is.na(sp))  { df$animal <- NA_character_; sp <- "animal" }
  if (any(is.na(c(src,reg,yr,val)))) stop("Missing required column(s). Seen: ", paste(names(df), collapse=", "))
  df %>%
    transmute(
      species = as.character(.data[[sp]]),
      region  = tolower(as.character(.data[[reg]])),
      year    = suppressWarnings(as.integer(.data[[yr]])),
      source  = as.character(.data[[src]]),
      value   = suppressWarnings(as.numeric(.data[[val]]))
    ) %>%
    filter(is.finite(year)) %>%
    mutate(value = ifelse(is.finite(value) & value <= 1, value*100, value),
           value = pmin(pmax(value,0),100))
}
map_sources <- function(df){
  df %>%
    mutate(src_clean = tolower(trimws(source))) %>%
    mutate(
      group = dplyr::case_when(
        str_detect(src_clean, "benthic") ~ "Benthic microalgae",
        str_detect(src_clean, "plankton") ~ "Plankton",
        str_detect(src_clean, "salt water cord grass|spartina") ~ "Spartina",
        str_detect(src_clean, "cat.?tail|typha") ~ "Cat-tail",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(group))
}
auto_find <- function(target, anchor){
  if (file.exists(target)) return(target)
  base <- dirname(normalizePath(anchor, winslash="/", mustWork=FALSE))
  guesses <- c(
    file.path(base, basename(target)),
    file.path(dirname(base), basename(target)),
    file.path(getwd(), basename(target))
  )
  hit <- guesses[file.exists(guesses)][1]
  if (!length(hit)) NA_character_ else hit
}
get_annual_sal <- function(annual_path, season_path, mix_path){
  ann_use <- auto_find(annual_path, mix_path)
  sea_use <- auto_find(season_path, mix_path)
  if (!is.na(ann_use) && file.exists(ann_use)) {
    msg("Using annual drivers: ", ann_use)
    ann <- readr::read_csv(ann_use, show_col_types = FALSE) %>% janitor::clean_names()
    sal_col <- pick_first(names(ann), c("sal_mean_annual","sal_annual_mean","salinity_annual_mean"))
    if (is.na(sal_col)) stop("Annual file lacks a salinity column like 'sal_mean_annual'. Columns: ", paste(names(ann), collapse=", "))
    ann %>%
      transmute(region = tolower(region), year = as.integer(year), sal = as.numeric(.data[[sal_col]])) %>%
      filter(region %in% c("lower","middle"), is.finite(year), is.finite(sal))
  } else {
    if (is.na(sea_use) || !file.exists(sea_use)) {
      stop("Could not find drivers files.\n  Tried annual: ", normalizePath(annual_path, winslash="/", mustWork=FALSE),
           "\n  Tried seasonal: ", normalizePath(season_path, winslash="/", mustWork=FALSE),
           "\n  Also searched near: ", normalizePath(mix_path, winslash="/", mustWork=FALSE))
    }
    msg("Annual not found; deriving annual salinity from seasonal: ", sea_use)
    sea <- readr::read_csv(sea_use, show_col_types = FALSE) %>% janitor::clean_names()
    sal_col <- pick_first(names(sea), c("sal_mean","sal"))
    if (is.na(sal_col)) stop("Seasonal file lacks 'sal_mean'. Columns: ", paste(names(sea), collapse=", "))
    sea %>%
      transmute(region = tolower(region), year = as.integer(year), sal = as.numeric(.data[[sal_col]])) %>%
      filter(region %in% c("lower","middle"), is.finite(year), is.finite(sal)) %>%
      group_by(region, year) %>% summarise(sal = mean(sal, na.rm = TRUE), .groups="drop")
  }
}

# ---------------- Load data ----------------
stopifnot(file.exists(mix_path))
mix_raw <- readr::read_csv(mix_path, show_col_types = FALSE)
mix <- standardize_mix(mix_raw) %>% map_sources() %>%
  mutate(species_label = species_map(species)) %>%
  filter(region %in% c("lower","middle"), !is.na(species_label), is.finite(value))

# %Plankton per species × region × year
plank_sry <- mix %>%
  filter(group == "Plankton") %>%
  group_by(species_label, region, year) %>%
  summarise(plankton_pct = sum(value, na.rm = TRUE), .groups="drop")

if (nrow(plank_sry) == 0) stop("After mapping sources, no rows for 'Plankton' remained. Check 'source' names.")

# salinity
ann <- get_annual_sal(annual_path, season_path, mix_path)

# Join & model columns (restrict to years with salinity so A/B align)
dat <- plank_sry %>%
  left_join(ann, by = c("region","year")) %>%
  filter(is.finite(sal)) %>%
  mutate(
    t_c    = year - break_year,
    Post   = as.integer(year >= break_year),
    t_post = pmax(0, t_c)
  )
if (nrow(dat) == 0) stop("No rows after joining salinity. Check region/year match between files.")

# ---------------- Significance: region-level step (bootstrap on medians) ----------------
set.seed(42)
reg_yr <- dat %>% group_by(region, year) %>%
  summarise(med_plank = median(plankton_pct, na.rm=TRUE), .groups="drop") %>%
  mutate(block = ifelse(year <= break_year-1, "Pre", "Post"))

boot_delta <- function(df, R=5000){
  pre  <- df$med_plank[df$block=="Pre"];  post <- df$med_plank[df$block=="Post"]
  if (length(pre) == 0 || length(post) == 0) return(c(est = NA_real_, lo = NA_real_, hi = NA_real_))
  deltas <- replicate(R, median(sample(post, length(post), TRUE)) -
                        median(sample(pre,  length(pre),  TRUE)))
  c(est = median(post, na.rm=TRUE) - median(pre, na.rm=TRUE),
    lo = as.numeric(quantile(deltas, .025)), hi = as.numeric(quantile(deltas, .975)))
}
reg_tests <- reg_yr %>%
  group_by(region) %>% do(as.data.frame(t(boot_delta(.)))) %>% ungroup()

msg("\nRegion-level Δ(Post−Pre) in %Plankton (medians, bootstrap 95% CI):")
print(reg_tests)

# --- Fiddler crab reference window (73% ± 13%) ---
fc_mean <- 73
fc_pm   <- 13
fc_lo   <- max(0,  fc_mean - fc_pm)   # 60
fc_hi   <- min(100, fc_mean + fc_pm)  # 86


# ---------------- Panel A: segmented fits on region-year medians ----------------
seg_fit_region <- function(df){
  df <- df %>% arrange(year)
  X <- data.frame(t_c = df$year - break_year,
                  Post = as.integer(df$year >= break_year),
                  t_post = pmax(0, df$year - break_year))
  fit <- try(stats::lm(med_plank ~ t_c + Post + t_post, data = cbind(df, X)), silent = TRUE)
  yrs <- seq(min(df$year), max(df$year))
  Xp  <- data.frame(t_c = yrs - break_year, Post = as.integer(yrs >= break_year),
                    t_post = pmax(0, yrs - break_year))
  if (inherits(fit, "try-error")) {
    return(tibble(year = yrs, fit = NA_real_, lo = NA_real_, hi = NA_real_))
  }
  p <- predict(fit, newdata = Xp, se.fit = TRUE)
  tibble(year = yrs, fit = as.numeric(p$fit),
         lo = as.numeric(p$fit - 1.96*p$se.fit),
         hi = as.numeric(p$fit + 1.96*p$se.fit))
}

reg_ts <- reg_yr %>% select(region, year, med_plank) %>%
  mutate(region = factor(region, levels = c("lower","middle")))
gridA <- reg_ts %>%
  group_by(region) %>%
  group_modify(~seg_fit_region(.x)) %>%
  ungroup() %>%
  mutate(region = factor(region, levels = c("lower","middle")))

pA <- ggplot() +
  # reference band & mean line
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = fc_lo, ymax = fc_hi,
           fill = "grey50", alpha = 0.10) +
  geom_hline(yintercept = fc_mean, linetype = 2, color = "grey40") +
  annotate("text",
           x = max(reg_ts$year, na.rm = TRUE),
           y = pmin(100, fc_hi + 2),
           label = "Fiddler crab 73% \u00B1 13%",
           hjust = 1, vjust = 0, size = 3, color = "grey30") +
  # data + fits
  geom_ribbon(data = gridA, aes(year, ymin = lo, ymax = hi, fill = region), alpha = 0.15, color = NA) +
  geom_line(data = gridA, aes(year, fit, color = region), linewidth = 1.1) +
  geom_point(data = reg_ts, aes(year, med_plank, color = region), size = 1.9, alpha = 0.85) +
  geom_vline(xintercept = break_year, linetype = 2, color = "#9A9A9A") +
  scale_color_manual(values = pal_region, name = "region", drop = FALSE) +
  scale_fill_manual(values  = pal_region, guide = "none", drop = FALSE) +
  labs(title = "A  %Plankton in diets (yearly medians) with 2014 segmented fits",
       x = NULL, y = "%Plankton (median across species)")


# ---------------- Panel B: all species, common year window, segmented fits ----------------
sp_ts <- dat %>%
  filter(species_label %in% c("Mummichog","Silversides","Softshell clam")) %>%
  select(species_label, region, year, plankton_pct)

if (nrow(sp_ts) == 0) stop("No species-level rows after filtering. Check species names in the mixing file.")

yr_window <- sp_ts %>% group_by(species_label, region) %>%
  summarise(miny = min(year), maxy = max(year), .groups = "drop")

yr_lo <- suppressWarnings(max(yr_window$miny))
yr_hi <- suppressWarnings(min(yr_window$maxy))
use_common <- is.finite(yr_lo) && is.finite(yr_hi) && yr_lo <= yr_hi

if (!use_common) {
  msg("Common year window across species×regions is empty; using full available window per data.")
  yr_lo <- min(sp_ts$year); yr_hi <- max(sp_ts$year)
}

sp_ts <- sp_ts %>% filter(year >= yr_lo, year <= yr_hi) %>%
  mutate(species_label = factor(species_label, levels = names(pal_species)),
         region        = factor(region, levels = c("lower","middle")))

seg_fit_sp <- function(df){
  df <- df %>% arrange(year)
  if (nrow(df) < 3) return(tibble(year = seq(yr_lo, yr_hi), fit = NA_real_, lo = NA_real_, hi = NA_real_))
  X <- data.frame(t_c = df$year - break_year, Post = as.integer(df$year >= break_year),
                  t_post = pmax(0, df$year - break_year))
  fit <- try(stats::lm(plankton_pct ~ t_c + Post + t_post, data = cbind(df, X)), silent = TRUE)
  yrs <- seq(yr_lo, yr_hi)
  Xp  <- data.frame(t_c = yrs - break_year, Post = as.integer(yrs >= break_year),
                    t_post = pmax(0, yrs - break_year))
  if (inherits(fit, "try-error")) return(tibble(year = yrs, fit = NA_real_, lo = NA_real_, hi = NA_real_))
  p <- predict(fit, newdata = Xp, se.fit = TRUE)
  tibble(year = yrs,
         fit = as.numeric(p$fit),
         lo  = as.numeric(p$fit - 1.96*p$se.fit),
         hi  = as.numeric(p$fit + 1.96*p$se.fit))
}
gridB <- sp_ts %>%
  group_by(species_label, region) %>%
  group_modify(~seg_fit_sp(.x)) %>%
  ungroup() %>%
  mutate(species_label = factor(species_label, levels = names(pal_species)),
         region        = factor(region, levels = c("lower","middle")))

pB_title <- if (use_common) {
  "B  %Plankton by species"
} else {
  "B  %Plankton by species"
}

pB <- ggplot() +
  # reference band & mean line
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = fc_lo, ymax = fc_hi,
           fill = "grey50", alpha = 0.10) +
  geom_hline(yintercept = fc_mean, linetype = 2, color = "grey40") +
  annotate("text",
           x = yr_hi,
           y = pmin(100, fc_hi + 2),
           label = "Fiddler crab 73% \u00B1 13%",
           hjust = 1, vjust = 0, size = 3, color = "grey30") +
  # data + fits
  geom_ribbon(data = gridB, aes(year, ymin = lo, ymax = hi, fill = species_label), alpha = 0.12, color = NA) +
  geom_line(data = gridB, aes(year, fit, color = species_label), linewidth = 0.9) +
  geom_point(data = sp_ts, aes(year, plankton_pct, color = species_label), size = 1.9, alpha = 0.85) +
  geom_vline(xintercept = break_year, linetype = 2, color = "#9A9A9A") +
  scale_color_manual(values = pal_species, name = "Species") +
  scale_fill_manual(values  = pal_species, guide = "none") +
  coord_cartesian(xlim = c(yr_lo, yr_hi), ylim = c(0, 100)) +
  labs(title = "B  %Plankton by species (common year window; segmented fits at 2014)",
       x = NULL, y = "%Plankton") +
  facet_grid(. ~ region) +
  theme(legend.position = "right")


# ---------------- Significance: per species × region step ----------------
step_tests <- sp_ts %>%
  mutate(t_c = year - break_year, Post = as.integer(year >= break_year), t_post = pmax(0, t_c)) %>%
  group_by(species_label, region) %>%
  group_modify(~{
    if (nrow(.x) < 5) return(tibble(estimate = NA_real_, se = NA_real_, df = NA_real_, t = NA_real_, p = NA_real_))
    fit <- lm(plankton_pct ~ t_c + Post + t_post, data = .x)
    s <- summary(fit)$coefficients
    tibble(estimate = unname(s["Post","Estimate"]),
           se       = unname(s["Post","Std. Error"]),
           df       = max(1, nrow(.x) - length(coef(fit))),
           t        = unname(s["Post","t value"]),
           p        = unname(s["Post","Pr(>|t|)"]))
  }) %>% ungroup() %>%
  mutate(sig = dplyr::case_when(
    is.na(p) ~ "",
    p < .001 ~ "***",
    p < .01  ~ "**",
    p < .05  ~ "*",
    TRUE     ~ "ns"
  ))

msg("\nPer-species×region 2014 step (Post) from segmented OLS:")
print(step_tests)

# ---------------- Compose + save (force high-res PNG, no ggsave) ----------------
fig <- pA / pB + patchwork::plot_layout(heights = c(0.58, 0.42)) +
  patchwork::plot_annotation(
    #title   = "Does increased marine influence elevate plankton use in diets?",
    #caption = "Segmented fits use a 2014 breakpoint. Panel A uses region-year medians; Panel B shows species-level series (Lower & Middle)."
  )

# Ensure a .png extension even if --out was passed as .pdf/.svg/.eps
out_png <- sub("\\.(pdf|svg|eps)$", ".png", out_png, ignore.case = TRUE)

# Write a crisp PNG (prefers ragg for best text/anti-aliasing)
if (requireNamespace("ragg", quietly = TRUE)) {
  ragg::agg_png(
    filename = out_png,
    width = 11.8, height = 7.8, units = "in", res = 600, background = "white"
  )
  print(fig)
  grDevices::dev.off()
} else {
  grDevices::png(
    filename = out_png,
    width = 11.8, height = 7.8, units = "in", res = 600, bg = "white", type = if (capabilities("cairo")) "cairo" else "Xlib"
  )
  print(fig)
  grDevices::dev.off()
}

msg("Saved figure (PNG): ", normalizePath(out_png, winslash="/", mustWork=FALSE))



# Always use our safe saver (avoid theme helper’s PDF-forcing behavior)
ggsave_smart(out_png, fig, width = 11.8, height = 7.8, dpi = 320, bg = "white")
msg("Saved figure: ", normalizePath(out_png, winslash="/", mustWork=FALSE))

# (Optionally also write a PDF next to it for QA)
out_pdf <- sub("\\.[A-Za-z0-9]+$",".pdf", out_png)
ggsave_smart(out_pdf, fig, width = 11.8, height = 7.8, dpi = 320, bg = "white")
msg("Saved PDF twin: ", normalizePath(out_pdf, winslash="/", mustWork=FALSE))

# Write helper tables
readr::write_csv(reg_yr %>% select(region, year, med_plank),
                 sub("\\.png$","_region_year_table.csv", out_png, ignore.case=TRUE))
readr::write_csv(step_tests,
                 sub("\\.png$","_species_region_step_tests.csv", out_png, ignore.case=TRUE))
readr::write_csv(reg_tests,
                 sub("\\.png$","_region_bootstrap_delta.csv", out_png, ignore.case=TRUE))

# Quick sanity logs
msg(sprintf("Panel A points (region-year medians): %d", nrow(reg_ts)))
msg(sprintf("Panel B points (species-region-year): %d", nrow(sp_ts)))
msg(sprintf("Year window used for Panel B: %s–%s (common=%s)", yr_lo, yr_hi, use_common))


