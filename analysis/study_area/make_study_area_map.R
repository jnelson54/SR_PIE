#!/usr/bin/env Rscript

install_if_missing <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    install.packages(missing, repos = "https://cloud.r-project.org")
  }
}

install_if_missing(c("ggplot2", "dplyr", "tibble", "sf"))

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tibble)
  library(sf)
})

get_script_dir <- function() {
  file_arg <- "--file="
  matched <- grep(file_arg, commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(matched) == 0) {
    return(normalizePath(getwd(), winslash = "/", mustWork = TRUE))
  }
  normalizePath(dirname(sub(file_arg, "", matched[1])), winslash = "/", mustWork = TRUE)
}

deg_label <- function(x, is_lon = FALSE) {
  hemi <- if (is_lon) ifelse(x < 0, "W", "E") else ifelse(x < 0, "S", "N")
  x_abs <- abs(x)
  deg <- floor(x_abs)
  mins <- round((x_abs - deg) * 60)
  carry <- mins == 60
  deg[carry] <- deg[carry] + 1
  mins[carry] <- 0
  sprintf("%d\u00B0%02d'%s", deg, mins, hemi)
}

km_to_lon <- function(km, latitude) {
  km / (111.32 * cos(latitude * pi / 180))
}

first_existing_dir <- function(paths) {
  hits <- paths[dir.exists(paths)]
  if (length(hits) == 0) {
    stop("Could not find PIE spatial layers needed for the study area map.")
  }
  normalizePath(hits[1], winslash = "/", mustWork = TRUE)
}

repo_root <- normalizePath(file.path(get_script_dir(), "..", ".."), winslash = "/", mustWork = TRUE)
figures_dir <- file.path(repo_root, "figures")
if (!dir.exists(figures_dir)) dir.create(figures_dir, recursive = TRUE)

source(file.path(repo_root, "analysis", "common", "theme_nyt.R"))

tables_dir <- first_existing_dir(c(
  file.path(repo_root, "analysis", "study_area", "data", "reports", "tables"),
  file.path(repo_root, "..", "..", "..", "PIE_LTER", "Nekton_Hypervolume", "pie_ml", "reports", "tables")
))

coast_dir <- first_existing_dir(c(
  file.path(repo_root, "analysis", "study_area", "data", "coast"),
  file.path(repo_root, "..", "..", "..", "PIE_Mixing_Model_2020", "PIE_Fiddler_Tethering", "mass_maps")
))

aoi_path <- file.path(tables_dir, "Estuary_AOI.shp")
coast_path <- file.path(coast_dir, "COAST25K_ARC.shp")

xlim <- c(-70.945, -70.742)
ylim <- c(42.664, 42.807)
mean_lat <- mean(ylim)

coast <- st_read(coast_path, quiet = TRUE) |>
  st_transform(4326)

coast <- st_as_sf(
  st_crop(
    st_geometry(coast),
    st_bbox(c(xmin = xlim[1], ymin = ylim[1], xmax = xlim[2], ymax = ylim[2]), crs = st_crs(4326))
  )
)

region_boxes <- tribble(
  ~region,   ~xmin,    ~xmax,    ~ymin,   ~ymax,
  "Upper",  -70.894,  -70.848,  42.742,  42.761,
  "Middle", -70.825,  -70.782,  42.721,  42.749,
  "Lower",  -70.807,  -70.765,  42.692,  42.716
)

label_positions <- tribble(
  ~region,   ~x,       ~y,
  "Upper",  -70.871,  42.7635,
  "Middle", -70.798,  42.752,
  "Lower",  -70.775,  42.7175
)

sound_polygon <- tibble(
  x = c(-70.925, -70.895, -70.865, -70.835, -70.807, -70.784, -70.768, -70.758, -70.764, -70.778, -70.796, -70.820, -70.848, -70.878, -70.908, -70.925),
  y = c(42.752, 42.764, 42.765, 42.759, 42.747, 42.731, 42.710, 42.692, 42.684, 42.688, 42.699, 42.715, 42.728, 42.737, 42.742, 42.752)
)

gulf_polygon <- tibble(
  x = c(-70.815, -70.742, -70.742, -70.754, -70.764, -70.775, -70.788, -70.800, -70.815),
  y = c(42.807, 42.807, 42.664, 42.664, 42.690, 42.714, 42.744, 42.778, 42.807)
)

north_arrow <- tribble(
  ~x,       ~y,
  -70.781,  42.797,
  -70.785,  42.791,
  -70.781,  42.793,
  -70.777,  42.791
)

scale_x0 <- -70.936
scale_y0 <- 42.6684
seg_lon <- km_to_lon(1, mean_lat)

scale_bar <- tibble(
  xmin = scale_x0 + seg_lon * c(0, 1, 2, 3),
  xmax = scale_x0 + seg_lon * c(1, 2, 3, 4),
  ymin = scale_y0,
  ymax = scale_y0 + 0.00125,
  fill = rep(c("#111111", "#FFFFFF"), length.out = 4)
)

scale_labels <- tibble(
  x = scale_x0 + seg_lon * c(0, 1, 2, 3, 4),
  y = scale_y0 + 0.0019,
  label = c("0", "1", "2", "3", "4 km")
)

lon_border_labels <- tibble(
  x = c(-70.93, -70.84, -70.75),
  y = ylim[1] + 0.004,
  label = deg_label(c(-70.93, -70.84, -70.75), is_lon = TRUE)
)

lat_border_labels <- tibble(
  x = xlim[1] + 0.004,
  y = c(42.68, 42.74, 42.80),
  label = deg_label(c(42.68, 42.74, 42.80), is_lon = FALSE)
)

p <- ggplot() +
  annotate(
    "rect",
    xmin = xlim[1], xmax = xlim[2], ymin = ylim[1], ymax = ylim[2],
    fill = "#B89E79", color = NA
  ) +
  geom_polygon(
    data = sound_polygon,
    aes(x, y),
    fill = "#D9EAF2",
    color = NA
  ) +
  geom_polygon(
    data = gulf_polygon,
    aes(x, y),
    fill = "#CFE1EC",
    color = NA
  ) +
  geom_sf(data = coast, color = "#2A2A2A", linewidth = 0.42) +
  geom_rect(
    data = region_boxes,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = NA,
    color = "#1A1A1A",
    linewidth = 0.55
  ) +
  geom_label(
    data = label_positions,
    aes(x, y, label = region),
    family = use_nyt_fonts(),
    fontface = "bold",
    size = 4.0,
    color = "#111111",
    fill = "#F8F7F2",
    linewidth = 0,
    label.padding = unit(0.08, "lines")
  ) +
  annotate(
    "label",
    x = -70.813,
    y = 42.729,
    label = "Plum Island Sound",
    family = use_nyt_fonts(),
    fontface = "bold",
    size = 3.0,
    color = "#111111",
    fill = "#F5F4EF",
    linewidth = 0,
    alpha = 0.9
  ) +
  annotate(
    "text",
    x = -70.747,
    y = 42.720,
    label = "Gulf of Maine",
    family = use_nyt_fonts(),
    size = 3.1,
    color = "#1F3A4A",
    hjust = 1,
    vjust = 0.5
  ) +
  geom_path(
    data = north_arrow,
    aes(x, y),
    color = "#111111",
    linewidth = 0.55,
    lineend = "round"
  ) +
  annotate(
    "text",
    x = -70.781,
    y = 42.800,
    label = "N",
    family = use_nyt_fonts(),
    fontface = "bold",
    size = 3.3,
    color = "#111111"
  ) +
  geom_rect(
    data = scale_bar,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = scale_bar$fill,
    color = "#111111",
    linewidth = 0.25
  ) +
  geom_text(
    data = scale_labels,
    aes(x, y, label = label),
    family = use_nyt_fonts(),
    size = 2.4,
    color = "#111111",
    vjust = 0,
    hjust = c(0.5, 0.5, 0.5, 0.5, 1)
  ) +
  geom_text(
    data = lon_border_labels,
    aes(x, y, label = label),
    family = use_nyt_fonts(),
    size = 2.6,
    color = "#111111",
    vjust = 1
  ) +
  geom_text(
    data = lat_border_labels,
    aes(x, y, label = label),
    family = use_nyt_fonts(),
    size = 2.6,
    color = "#111111",
    hjust = 0
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE, clip = "off", datum = NA) +
  labs(title = "Plum Island Estuary study area") +
  theme_nyt(gridline_x = FALSE, gridline_y = FALSE, base_size = 11) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "#111111", fill = NA, linewidth = 0.8),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 16, face = "bold", color = "#333333", margin = margin(b = 8)),
    legend.position = "none",
    plot.margin = margin(12, 14, 8, 14)
  )

out_pdf <- file.path(figures_dir, "fig_study_area_map.pdf")
out_png <- file.path(figures_dir, "fig_study_area_map.png")

save_nyt(out_pdf, p, width = 6.6, height = 5.8)
save_nyt(out_png, p, width = 6.6, height = 5.8, dpi = 400)

message("Wrote: ", out_pdf)
message("Wrote: ", out_png)