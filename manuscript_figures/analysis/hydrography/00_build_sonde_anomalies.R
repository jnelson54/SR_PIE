#!/usr/bin/env Rscript
# Build the seasonal temperature and conductivity anomalies used in Figure 2
# from the high-tide sonde extract.
#
# Input:  High Tide Values.csv  (high-tide temperature and conductivity at IBYC
#         [lower estuary] and Middle Road Bridge [MB, upper estuary], 2000-2025,
#         extracted from the PIE LTER 15-minute sonde packages listed in
#         repro/manifests/full_estuarine_moored_sonde_packages.csv)
# Output: pie_temp_sal_sonde.csv (Year, Season, Temp, Cond, Site, Region)
#
# Rules (these reproduce the anomaly table used for the submitted manuscript):
#   1. Keep records in which temperature and conductivity are both finite.
#   2. Keep May-October. Season 1 = May-June; Season 2 = July-October.
#   3. For each station x year x season, average the high-tide records.
#   4. Baseline = mean of those year-by-season means across all years, by
#      station and season. Anomaly = year-by-season mean minus baseline.
# No outlier removal and no minimum-sample filter are applied.

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(lubridate)
})

get_script_dir <- function() {
  matched <- grep("--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(matched) == 0) return(normalizePath(getwd(), winslash = "/"))
  normalizePath(dirname(sub("--file=", "", matched[1])), winslash = "/")
}
script_dir <- get_script_dir()

raw <- read_csv(file.path(script_dir, "High Tide Values.csv"),
                show_col_types = FALSE, name_repair = "unique_quiet")

station <- function(date_col, temp_col, cond_col, site, region) {
  tibble(
    datetime = parse_date_time(raw[[date_col]],
                               orders = c("m/d/y I:M p", "m/d/Y I:M p", "m/d/Y", "m/d/y"),
                               quiet = TRUE),
    Temp = suppressWarnings(as.numeric(raw[[temp_col]])),
    Cond = suppressWarnings(as.numeric(raw[[cond_col]])),
    Site = site, Region = region
  )
}

obs <- bind_rows(
  station(2, 3, 4, "IBYC", "Lower"),
  station(7, 8, 9, "MB", "Upper")
) %>%
  filter(!is.na(datetime), is.finite(Temp), is.finite(Cond)) %>%
  mutate(Year = year(datetime), Month = month(datetime)) %>%
  filter(Month %in% 5:10) %>%
  mutate(Season = if_else(Month %in% 5:6, 1L, 2L))

season_means <- obs %>%
  group_by(Site, Region, Year, Season) %>%
  summarise(Temp = mean(Temp), Cond = mean(Cond), n_high_tides = n(), .groups = "drop")

anomalies <- season_means %>%
  group_by(Site, Season) %>%
  mutate(Temp = Temp - mean(Temp), Cond = Cond - mean(Cond)) %>%
  ungroup() %>%
  arrange(Site, Year, Season)

write_csv(anomalies %>% select(Year, Season, Temp, Cond, Site, Region),
          file.path(script_dir, "pie_temp_sal_sonde.csv"))
write_csv(season_means %>% arrange(Site, Year, Season),
          file.path(script_dir, "pie_sonde_season_means_and_counts.csv"))

# Check against the anomaly table used for the submitted manuscript.
ref_path <- file.path(script_dir, "pie_temp_sal_sonde_as_submitted.csv")
if (file.exists(ref_path)) {
  ref <- read_csv(ref_path, show_col_types = FALSE)
  chk <- inner_join(anomalies, ref, by = c("Site", "Year", "Season"), suffix = c("", "_ref"))
  stopifnot(nrow(chk) == nrow(ref), nrow(chk) == nrow(anomalies))
  d_temp <- max(abs(chk$Temp - chk$Temp_ref)); d_cond <- max(abs(chk$Cond - chk$Cond_ref))
  message(sprintf("Rebuilt %d station-year-season anomalies; max abs difference from submitted table: Temp %.2e, Cond %.2e",
                  nrow(chk), d_temp, d_cond))
  stopifnot(d_temp < 1e-9, d_cond < 1e-9)
}
