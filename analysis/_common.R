ensure_packages <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    message("Installing missing packages: ", paste(missing, collapse = ", "))
    install.packages(missing, repos = "https://cloud.r-project.org")
  }
  invisible(TRUE)
}

get_project_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_flag <- "--file="
  file_arg <- args[grepl(file_flag, args)]
  if (length(file_arg) > 0) {
    script_path <- normalizePath(sub(file_flag, "", file_arg[1]), winslash = "/", mustWork = FALSE)
    return(normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE))
  }
  normalizePath(".", winslash = "/", mustWork = FALSE)
}

safe_mean <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  mean(x, na.rm = TRUE)
}

infer_site_code <- function(file_name, data) {
  fn <- toupper(basename(file_name))
  m <- regexec("_YSI_([A-Z0-9]+)\\.CSV$", fn)
  mm <- regmatches(fn, m)[[1]]
  if (length(mm) == 2) {
    return(mm[2])
  }
  if (grepl("MON-PR-MBYSI", fn, fixed = TRUE)) return("MB")
  if (grepl("MON-PR-RRYSI", fn, fixed = TRUE)) return("RR")
  if (grepl("MON-SO-IBYCYSI", fn, fixed = TRUE)) return("IBYC")
  if (grepl("MON-PR-RT1AYSI", fn, fixed = TRUE)) return("RT1A")

  if ("Station" %in% names(data)) {
    stations <- toupper(as.character(stats::na.omit(data$Station)))
    if (length(stations) > 0) {
      s <- stations[1]
      if (grepl("EST-RO", s, fixed = TRUE)) return("RO")
      if (grepl("EST-PR", s, fixed = TRUE)) return("PR")
    }
  }
  "UNKNOWN"
}

combined_site_from_code <- function(site_code) {
  dplyr::case_when(
    site_code %in% c("IBYC", "IB") ~ "LOWER_MAIN",
    site_code %in% c("MB", "MR") ~ "MIDDLE_MAIN",
    TRUE ~ NA_character_
  )
}

infer_datetime <- function(df) {
  nms <- names(df)
  nms_low <- tolower(nms)

  # Priority: explicit timestamp-like column.
  ts_idx <- match(TRUE, nms_low %in% c("timestamp", "datetime", "date_time", "sampledatetime", "dateandtime"))
  if (!is.na(ts_idx)) {
    x <- trimws(as.character(df[[nms[ts_idx]]]))
    dt <- suppressWarnings(lubridate::parse_date_time(
      x,
      orders = c("Y-m-d H:M:S", "Y-m-d H:M", "m/d/Y H:M:S", "m/d/Y H:M", "d-b-Y H:M:S", "d-b-Y H:M"),
      tz = "UTC",
      exact = FALSE
    ))
    if (any(!is.na(dt))) {
      return(as.POSIXct(dt, tz = "UTC"))
    }
  }

  date_idx <- match(TRUE, grepl("\\bdate\\b", nms_low))
  time_idx <- match(TRUE, grepl("\\btime\\b", nms_low))
  if (is.na(date_idx)) {
    return(as.POSIXct(rep(NA, nrow(df)), origin = "1970-01-01", tz = "UTC"))
  }

  date_chr <- trimws(as.character(df[[nms[date_idx]]]))
  date_sample <- head(date_chr, 200)
  iso_like <- mean(grepl("^\\d{4}-\\d{2}-\\d{2}$", date_sample)) > 0.8
  textual_month <- mean(grepl("[A-Za-z]{3}", date_sample)) > 0.2
  use_dayfirst <- isTRUE(textual_month && !iso_like)

  parse_orders <- if (use_dayfirst) {
    c(
      "d-b-Y H:M:S", "d-b-Y H:M", "d-B-Y H:M:S", "d-B-Y H:M",
      "d-b-y H:M:S", "d-b-y H:M", "d-B-y H:M:S", "d-B-y H:M",
      "d/m/Y H:M:S", "d/m/Y H:M", "d/m/y H:M:S", "d/m/y H:M",
      "Y-m-d H:M:S", "Y-m-d H:M"
    )
  } else {
    c(
      "Y-m-d H:M:S", "Y-m-d H:M",
      "m/d/Y H:M:S", "m/d/Y H:M", "m/d/y H:M:S", "m/d/y H:M",
      "d-b-Y H:M:S", "d-b-Y H:M", "d-B-Y H:M:S", "d-B-Y H:M",
      "d-b-y H:M:S", "d-b-y H:M", "d-B-y H:M:S", "d-B-y H:M"
    )
  }

  if (!is.na(time_idx)) {
    time_chr <- trimws(as.character(df[[nms[time_idx]]]))
    combo <- paste(date_chr, time_chr)
    dt <- suppressWarnings(lubridate::parse_date_time(combo, orders = parse_orders, tz = "UTC", exact = FALSE))

    # Try numeric Excel-style times if needed.
    missing <- is.na(dt)
    if (any(missing)) {
      d_only <- suppressWarnings(lubridate::parse_date_time(
        date_chr,
        orders = c("d-b-Y", "d-B-Y", "d-b-y", "d-B-y", "Y-m-d", "m/d/Y", "m/d/y", "d/m/Y", "d/m/y"),
        tz = "UTC",
        exact = FALSE
      ))
      t_num <- suppressWarnings(as.numeric(time_chr))
      frac <- missing & !is.na(t_num) & t_num >= 0 & t_num < 1 & !is.na(d_only)
      if (any(frac)) {
        dt[frac] <- as.POSIXct(d_only[frac], tz = "UTC") + t_num[frac] * 86400
      }
      serial <- missing & !is.na(t_num) & t_num >= 1 & t_num < 100000
      if (any(serial)) {
        dt[serial] <- as.POSIXct("1899-12-30", tz = "UTC") + t_num[serial] * 86400
      }
    }
    return(as.POSIXct(dt, tz = "UTC"))
  }

  dt <- suppressWarnings(lubridate::parse_date_time(
    date_chr,
    orders = c("d-b-Y", "d-B-Y", "d-b-y", "d-B-y", "Y-m-d", "m/d/Y", "m/d/y", "d/m/Y", "d/m/y"),
    tz = "UTC",
    exact = FALSE
  ))
  as.POSIXct(dt, tz = "UTC")
}

pick_col <- function(nms, patterns) {
  nms_low <- tolower(nms)
  for (p in patterns) {
    idx <- which(grepl(p, nms_low, perl = TRUE))
    if (length(idx) > 0) {
      return(nms[idx[1]])
    }
  }
  NA_character_
}

extract_numeric <- function(df, patterns) {
  col <- pick_col(names(df), patterns)
  if (is.na(col)) {
    return(rep(NA_real_, nrow(df)))
  }
  suppressWarnings(as.numeric(df[[col]]))
}

canonical_presence <- function(df_subset) {
  nms <- names(df_subset)
  out <- c(
    temperature = FALSE,
    salinity = FALSE,
    conductivity = FALSE,
    dissolved_oxygen = FALSE,
    depth = FALSE,
    ph = FALSE,
    turbidity = FALSE,
    chlorophyll_algae = FALSE,
    fdom_fluorescence = FALSE
  )

  bucket_has <- function(patterns) {
    for (p in patterns) {
      idx <- which(grepl(p, tolower(nms), perl = TRUE))
      if (length(idx) == 0) next
      for (i in idx) {
        v <- suppressWarnings(as.numeric(df_subset[[nms[i]]]))
        if (any(!is.na(v))) return(TRUE)
      }
    }
    FALSE
  }

  out["temperature"] <- bucket_has(c("^temp$", "temperature"))
  out["salinity"] <- bucket_has(c("^salinity$", "calcsalinity"))
  out["conductivity"] <- bucket_has(c("^spcond$", "^cond$", "conduct"))
  out["dissolved_oxygen"] <- bucket_has(c("^do", "^odo", "dissolved oxygen"))
  out["depth"] <- bucket_has(c("depth", "^pressure$"))
  out["ph"] <- bucket_has(c("^ph$", "^ph_", "_ph$", "ph_m?v"))
  out["turbidity"] <- bucket_has(c("turbid"))
  out["chlorophyll_algae"] <- bucket_has(c("chlorophyll", "phyco", "algae"))
  out["fdom_fluorescence"] <- bucket_has(c("fdom", "fluores"))
  out
}

theil_sen_slope <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  n <- length(x)
  if (n < 2) return(NA_real_)
  slopes <- c()
  for (i in seq_len(n - 1)) {
    for (j in seq((i + 1), n)) {
      if (x[j] != x[i]) {
        slopes <- c(slopes, (y[j] - y[i]) / (x[j] - x[i]))
      }
    }
  }
  if (length(slopes) == 0) return(NA_real_)
  stats::median(slopes, na.rm = TRUE)
}

bootstrap_theil_sen_ci <- function(x, y, b = 2000, conf = 0.95, seed = 123) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  n <- length(x)
  if (n < 6) {
    return(c(lcl = NA_real_, ucl = NA_real_))
  }
  set.seed(seed)
  boot <- numeric(b)
  for (i in seq_len(b)) {
    idx <- sample.int(n, size = n, replace = TRUE)
    boot[i] <- theil_sen_slope(x[idx], y[idx])
  }
  alpha <- (1 - conf) / 2
  stats::quantile(boot, probs = c(alpha, 1 - alpha), na.rm = TRUE, names = FALSE)
}
