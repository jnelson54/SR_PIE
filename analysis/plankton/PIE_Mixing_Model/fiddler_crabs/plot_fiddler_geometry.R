#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(purrr)
  library(tibble)
})

args <- commandArgs(trailingOnly = TRUE)
default_folder <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
folder <- if (length(args) >= 1) args[1] else default_folder
folder <- normalizePath(folder, winslash = "/", mustWork = FALSE)

input_dir <- file.path(folder, "model_suite_inputs")
output_dir <- file.path(folder, "model_suite_outputs", "geometry_plots")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

mix_file <- file.path(folder, "fiddler_crabs.csv")
raw_sources_file <- file.path(input_dir, "source_rows_rebuild.csv")
source_means_file <- file.path(input_dir, "sources_rebuild_3iso.csv")
tef_standard_file <- file.path(input_dir, "tef_standard.csv")
tef_pathway_file <- file.path(input_dir, "tef_pathway.csv")

consumers <- readr::read_csv(mix_file, show_col_types = FALSE)
raw_sources <- readr::read_csv(raw_sources_file, show_col_types = FALSE)
source_means <- readr::read_csv(source_means_file, show_col_types = FALSE)
tef_standard <- readr::read_csv(tef_standard_file, show_col_types = FALSE) %>% dplyr::mutate(tef_scheme = "Standard TEF")
tef_pathway <- readr::read_csv(tef_pathway_file, show_col_types = FALSE) %>% dplyr::mutate(tef_scheme = "Pathway TEF")
tef_all <- dplyr::bind_rows(tef_standard, tef_pathway)

source_order <- c("benthic microalgae", "marsh plants", "pelagic zooplankton")
source_colors <- c(
  "benthic microalgae" = "#1f7a8c",
  "marsh plants" = "#c06c2b",
  "pelagic zooplankton" = "#6c5b7b"
)

raw_sources <- raw_sources %>%
  dplyr::mutate(source_group = factor(source_group, levels = source_order))

source_means <- source_means %>%
  dplyr::mutate(Sources = factor(Sources, levels = source_order)) %>%
  dplyr::rename(
    source_d15N = Meand15N,
    source_d13C = Meand13C,
    source_d34S = Meand34S
  )

tef_all <- tef_all %>%
  dplyr::rename(
    tef_d15N = Meand15N,
    tef_d13C = Meand13C,
    tef_d34S = Meand34S
  )

corrected_means <- source_means %>%
  dplyr::left_join(tef_all, by = "Sources") %>%
  dplyr::transmute(
    tef_scheme,
    source = factor(Sources, levels = source_order),
    raw_d13C = source_d13C,
    raw_d15N = source_d15N,
    raw_d34S = source_d34S,
    corr_d13C = source_d13C + tef_d13C,
    corr_d15N = source_d15N + tef_d15N,
    corr_d34S = source_d34S + tef_d34S
  )

inside_triangle <- function(points, triangle) {
  sign_val <- function(p1, p2, p3) {
    (p1[1] - p3[1]) * (p2[2] - p3[2]) - (p2[1] - p3[1]) * (p1[2] - p3[2])
  }
  apply(points, 1, function(point) {
    d1 <- sign_val(point, triangle[1, ], triangle[2, ])
    d2 <- sign_val(point, triangle[2, ], triangle[3, ])
    d3 <- sign_val(point, triangle[3, ], triangle[1, ])
    has_neg <- any(c(d1, d2, d3) < 0)
    has_pos <- any(c(d1, d2, d3) > 0)
    !(has_neg && has_pos)
  })
}

consumer_2iso <- consumers %>% dplyr::select(d13C, d34S)

triangle_counts <- corrected_means %>%
  dplyr::group_by(tef_scheme) %>%
  dplyr::group_modify(~ {
    tri <- .x %>% dplyr::arrange(match(source, source_order)) %>% dplyr::select(corr_d13C, corr_d34S) %>% as.matrix()
    inside <- inside_triangle(as.matrix(consumer_2iso), tri)
    tibble::tibble(inside_n = sum(inside), total_n = length(inside))
  }) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(label = paste0("Inside mean triangle: ", inside_n, "/", total_n))

corrected_means_2iso <- corrected_means %>%
  dplyr::left_join(triangle_counts, by = "tef_scheme") %>%
  dplyr::mutate(source = factor(source, levels = source_order))

polygon_2iso <- corrected_means_2iso %>%
  dplyr::arrange(tef_scheme, match(source, source_order))

consumer_long <- consumers %>%
  dplyr::select(num, d13C, d15N, d34S) %>%
  tidyr::pivot_longer(cols = c(d13C, d15N, d34S), names_to = "isotope", values_to = "value")

raw_sources_long <- raw_sources %>%
  dplyr::select(source_group, d13C, d15N, d34S) %>%
  tidyr::pivot_longer(cols = c(d13C, d15N, d34S), names_to = "isotope", values_to = "value")

pair_specs <- tibble::tribble(
  ~x_var, ~y_var, ~panel_label,
  "corr_d13C", "corr_d34S", "d13C vs d34S",
  "corr_d13C", "corr_d15N", "d13C vs d15N",
  "corr_d15N", "corr_d34S", "d15N vs d34S"
)

pairwise_polygons <- purrr::pmap_dfr(
  pair_specs,
  function(x_var, y_var, panel_label) {
    corrected_means %>%
      dplyr::transmute(
        tef_scheme,
        source,
        panel_label = panel_label,
        x = .data[[x_var]],
        y = .data[[y_var]]
      )
  }
) %>%
  dplyr::arrange(tef_scheme, panel_label, match(source, source_order))

pairwise_consumers <- purrr::pmap_dfr(
  pair_specs,
  function(x_var, y_var, panel_label) {
    consumers %>%
      dplyr::transmute(
        panel_label = panel_label,
        x = .data[[sub("corr_", "", x_var)]],
        y = .data[[sub("corr_", "", y_var)]]
      )
  }
)

raw_plot <- ggplot() +
  geom_point(
    data = raw_sources %>% dplyr::filter(!is.na(d34S)),
    aes(x = d13C, y = d34S, color = source_group),
    size = 2.7,
    alpha = 0.75
  ) +
  geom_point(
    data = consumers,
    aes(x = d13C, y = d34S),
    size = 2.8,
    shape = 21,
    stroke = 0.5,
    fill = "#111111",
    color = "white"
  ) +
  scale_color_manual(values = source_colors, drop = FALSE) +
  labs(
    title = "Raw rebuild inputs in d13C-d34S space",
    subtitle = "Colored points are raw source observations used in the rebuild; black points are 2023 and 2025 fiddler crabs",
    x = "d13C (‰)",
    y = "d34S (‰)",
    color = "Raw source group"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

triangle_plot <- ggplot() +
  geom_polygon(
    data = polygon_2iso,
    aes(x = corr_d13C, y = corr_d34S, group = tef_scheme),
    fill = "#9ecae1",
    alpha = 0.18,
    color = "#2b2d42",
    linewidth = 0.8
  ) +
  geom_segment(
    data = corrected_means_2iso,
    aes(x = raw_d13C, y = raw_d34S, xend = corr_d13C, yend = corr_d34S, color = source),
    linewidth = 0.7,
    alpha = 0.8,
    arrow = arrow(length = grid::unit(0.18, "cm"), type = "closed")
  ) +
  geom_point(
    data = consumers,
    aes(x = d13C, y = d34S),
    size = 2.7,
    shape = 21,
    stroke = 0.4,
    fill = "#111111",
    color = "white"
  ) +
  geom_point(
    data = corrected_means_2iso,
    aes(x = corr_d13C, y = corr_d34S, color = source),
    size = 4
  ) +
  geom_point(
    data = corrected_means_2iso,
    aes(x = raw_d13C, y = raw_d34S, color = source),
    shape = 1,
    size = 4,
    stroke = 1.1
  ) +
  geom_text(
    data = corrected_means_2iso,
    aes(x = corr_d13C, y = corr_d34S, label = source, color = source),
    nudge_y = 1.2,
    size = 3.4,
    show.legend = FALSE
  ) +
  geom_text(
    data = triangle_counts,
    aes(x = -22.9, y = 18.2, label = label),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3.6,
    color = "#2b2d42"
  ) +
  facet_wrap(~ tef_scheme) +
  scale_color_manual(values = source_colors, drop = FALSE) +
  labs(
    title = "TEF-shifted mean-source triangles remain open",
    subtitle = "Open circles are raw source means; filled points are TEF-corrected means; arrows show the applied TEF shift",
    x = "d13C (‰)",
    y = "d34S (‰)",
    color = "Source"
  ) +
  coord_cartesian(xlim = c(-24.5, -10), ylim = c(-18.5, 18.8)) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

pairwise_plot <- ggplot() +
  geom_polygon(
    data = pairwise_polygons,
    aes(x = x, y = y, group = interaction(tef_scheme, panel_label)),
    fill = "#f4d19b",
    alpha = 0.18,
    color = "#2b2d42",
    linewidth = 0.8
  ) +
  geom_point(
    data = pairwise_consumers,
    aes(x = x, y = y),
    size = 2.3,
    shape = 21,
    stroke = 0.35,
    fill = "#111111",
    color = "white"
  ) +
  geom_point(
    data = pairwise_polygons,
    aes(x = x, y = y, color = source),
    size = 3.6
  ) +
  facet_grid(tef_scheme ~ panel_label, scales = "free") +
  scale_color_manual(values = source_colors, drop = FALSE) +
  labs(
    title = "Pairwise projections of the TEF-shifted mean-source geometry",
    subtitle = "These 2D projections are useful for inspection, but pairwise overlap alone does not prove 3D closure",
    x = NULL,
    y = NULL,
    color = "Source"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

ggsave(file.path(output_dir, "raw_points.png"), raw_plot, width = 9, height = 6.5, dpi = 300, device = "png")
ggsave(file.path(output_dir, "fiddler_geometry_raw_points_2iso.pdf"), raw_plot, width = 9, height = 6.5, dpi = 300)
ggsave(file.path(output_dir, "mean_triangles.png"), triangle_plot, width = 12, height = 6.8, dpi = 300, device = "png")
ggsave(file.path(output_dir, "fiddler_geometry_mean_triangles.pdf"), triangle_plot, width = 12, height = 6.8, dpi = 300)
ggsave(file.path(output_dir, "pairwise_3iso.png"), pairwise_plot, width = 14, height = 8.5, dpi = 300, device = "png")
ggsave(file.path(output_dir, "fiddler_geometry_pairwise_3iso.pdf"), pairwise_plot, width = 14, height = 8.5, dpi = 300)
readr::write_csv(corrected_means_2iso, file.path(output_dir, "fiddler_geometry_corrected_means.csv"))

cat("Geometry plots written to:\n  ", output_dir, "\n", sep = "")