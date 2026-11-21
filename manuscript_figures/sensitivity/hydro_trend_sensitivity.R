suppressPackageStartupMessages({library(dplyr); library(tidyr); library(readr); library(lubridate); library(purrr)})
H <- file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])), "..", "analysis", "hydrography")
raw <- read_csv(file.path(H, "High Tide Values.csv"), show_col_types = FALSE, name_repair = "unique_quiet")
mk <- function(dcol, tcol, ccol, site, region) tibble(dt = parse_date_time(raw[[dcol]], orders = c("m/d/y I:M p","m/d/Y I:M p","m/d/Y","m/d/y"), quiet = TRUE),
   Temp = as.numeric(raw[[tcol]]), Cond = as.numeric(raw[[ccol]]), Site = site, Region = region) %>% filter(!is.na(dt))
d <- bind_rows(mk(2,3,4,"IBYC","Lower"), mk(7,8,9,"MB","Upper")) %>% mutate(Year = year(dt), Month = month(dt)) %>% filter(is.finite(Temp), is.finite(Cond))
cat("complete-case rows:", nrow(d), "\n")
tr <- function(y, t) { ok <- is.finite(y) & is.finite(t); y <- y[ok]; t <- t[ok]; z <- zyp::zyp.trend.vector(y, t, method = "yuepilon"); c(slope_dec = unname(z["trend"])*10, lo = unname(z["lbound"])*10, hi = unname(z["ubound"])*10, p = unname(z["sig"]), n = length(y)) }
bins <- d %>% filter(Month %in% 5:10) %>% mutate(Season = if_else(Month %in% 5:6, 1L, 2L)) %>% group_by(Region, Site, Year, Season) %>% summarise(Temp = mean(Temp), Cond = mean(Cond), n = n(), .groups = "drop") %>%
  group_by(Region, Season) %>% mutate(Temp = Temp - mean(Temp), Cond = Cond - mean(Cond)) %>% ungroup() %>% mutate(time = Year + (Season - 1)/2)
ref <- read_csv(file.path(H, "pie_temp_sal_sonde.csv"), show_col_types = FALSE)
chk <- bins %>% inner_join(ref, by = c("Site","Year","Season"), suffix = c("", "_ref"))
cat("rebuild vs archived anomalies: n =", nrow(chk), "of", nrow(ref), " max|dTemp| =", max(abs(chk$Temp - chk$Temp_ref)), " max|dCond| =", max(abs(chk$Cond - chk$Cond_ref)), "\n")
print(bins %>% group_by(Region) %>% summarise(nbins = n(), min_n = min(n), lt20 = sum(n < 20), years = n_distinct(Year)))
print(bins %>% filter(n < 20))
out <- list()
add <- function(label, df, tcol = "time") for (r in c("Lower","Upper")) for (v in c("Temp","Cond")) { x <- df[df$Region == r, ]; out[[length(out)+1]] <<- c(list(analysis = label, Region = r, variable = v), as.list(round(tr(x[[v]], x[[tcol]]), 4))) }
add("A. as published (two bins/yr, May-Jun & Jul-Oct)", bins)
add("B. drop bins with <20 high-tide obs", bins %>% filter(n >= 20))
ann <- bins %>% group_by(Region, Year) %>% summarise(Temp = mean(Temp), Cond = mean(Cond), .groups = "drop")
add("C. one value/yr (mean of the two bin anomalies)", ann, "Year")
mon <- d %>% filter(Month %in% 4:11) %>% group_by(Region, Year, Month) %>% summarise(Temp = mean(Temp), Cond = mean(Cond), n = n(), .groups = "drop") %>% filter(n >= 10) %>%
  group_by(Region, Month) %>% mutate(Temp = Temp - mean(Temp), Cond = Cond - mean(Cond)) %>% ungroup() %>% group_by(Region, Year) %>% summarise(Temp = mean(Temp), Cond = mean(Cond), nm = n(), .groups = "drop")
add("D. Apr-Nov, monthly climatology, annual mean of monthly anomalies", mon, "Year")
res <- bind_rows(lapply(out, as_tibble)) %>% group_by(analysis) %>% mutate(q = round(p.adjust(p, "BH"), 4)) %>% ungroup()
print(as.data.frame(res), row.names = FALSE)
write_csv(res, file.path(H, "..", "..", "sensitivity", "hydro_trend_sensitivity.csv"))
# raw ranges
print(d %>% filter(Month %in% 5:10) %>% group_by(Region) %>% summarise(Tmean = mean(Temp), Cmean = mean(Cond), C05 = quantile(Cond, .05), C95 = quantile(Cond, .95), T05 = quantile(Temp,.05), T95 = quantile(Temp,.95), n = n()))
