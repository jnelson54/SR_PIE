get_run_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_flag <- "--file="
  file_arg <- args[grepl(file_flag, args)]
  if (length(file_arg) > 0) {
    script_path <- normalizePath(sub(file_flag, "", file_arg[1]), winslash = "/", mustWork = FALSE)
    return(normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE))
  }
  normalizePath(".", winslash = "/", mustWork = FALSE)
}

root <- get_run_root()
scripts <- c(
  file.path(root, "analysis", "01_prepare_sonde_seasonal_data.R"),
  file.path(root, "analysis", "02_fit_trends_and_anomalies.R"),
  file.path(root, "analysis", "03_make_publication_figures.R")
)

for (s in scripts) {
  if (!file.exists(s)) {
    stop("Required script not found: ", s)
  }
}

for (s in scripts) {
  message("Running: ", s)
  sys.source(s, envir = new.env(parent = globalenv()))
}

message("Complete. Outputs are in analysis/output/sonde and analysis/figures.")
