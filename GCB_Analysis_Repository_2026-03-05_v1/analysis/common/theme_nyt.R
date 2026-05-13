suppressPackageStartupMessages({
  library(ggplot2)
  library(grid) # for unit()
})

# Minimal font setup helper --------------------------------------------
use_nyt_fonts <- function(base_family = "Libre Franklin", fallback = "Arial") {
  # If Libre Franklin isn't installed, silently fall back to system sans
  fams <- try(systemfonts::system_fonts()$family, silent = TRUE)
  if (inherits(fams, "try-error") || !(base_family %in% fams)) {
    message(sprintf("'%s' not found. Using '%s'.", base_family, fallback))
    base_family <- fallback
  }
  base_family
}

# NYT theme -------------------------------------------------------------
theme_nyt <- function(gridline_x = FALSE,
                      gridline_y = TRUE,
                      base_family = "Libre Franklin",
                      base_size   = 12) {
  base_family <- use_nyt_fonts(base_family)

  gridline <- element_line(linetype = "dashed", linewidth = 0.15, color = "#999999")
  gx <- if (isTRUE(gridline_x)) gridline else element_blank()
  gy <- if (isTRUE(gridline_y)) gridline else element_blank()

  theme_minimal(base_family = base_family, base_size = base_size) +
    theme(
      # Text
      plot.title   = element_text(size = 18, face = "bold",  color = "#333333", margin = margin(b = 10)),
      plot.subtitle= element_text(size = 14,                 color = "#999999", margin = margin(b = 10)),
      plot.caption = element_text(size = 11,                 color = "#777777", margin = margin(t = 15), hjust = 0),
      axis.title   = element_text(size = 12, color = "#555555"),
      axis.text    = element_text(size = 11, color = "#333333"),
      plot.title.position   = "plot",
      plot.caption.position = "plot",

      # Gridlines and ticks
      panel.grid.minor = element_blank(),
      panel.grid.major.x = gx,
      panel.grid.major.y = gy,
      axis.ticks.x = element_line(linetype = "solid", linewidth = 0.25, color = "#999999"),
      axis.ticks.length.x = unit(4, "pt"),

      # Legend
      legend.position = "top",
      legend.title = element_text(size = 11, color = "#444444"),
      legend.text  = element_text(size = 10, color = "#444444"),

      # Margins for breathing room
      plot.margin = margin(12, 16, 12, 16)
    )
}

# ---- palettes (colorblind-safe) ----
pal_oi <- c(
  blue  = "#0072B2", orange = "#E69F00", teal = "#009E73",
  sky   = "#56B4E9", red    = "#D55E00", purple="#CC79A7",
  yellow= "#F0E442", black  = "#000000", tan = "#CEB888", maroon = "#782F40"
)

# Diverging for signed continuous (tan-white-maroon)
scale_fill_diverging_pub <- function(name = NULL, limits = NULL, midpoint = 0) {
  ggplot2::scale_fill_gradient2(
    name = name, low = pal_oi["tan"], mid = "#FFFFFF", high = pal_oi["maroon"],
    midpoint = midpoint, limits = limits, na.value = "#f5f5f5",
    guide = guide_colorbar(barheight = grid::unit(80, "pt"), ticks.colour = NA)
  )
}

# Save publication-ready figures; PDF uses vector output, raster uses requested dpi.
save_nyt <- function(filename, plot, width, height, dpi = 300, bg = "white") {
  ext <- tolower(tools::file_ext(filename))

  if (ext == "pdf") {
    dev <- if (capabilities("cairo")) grDevices::cairo_pdf else "pdf"
    ggplot2::ggsave(
      filename = filename,
      plot = plot,
      width = width,
      height = height,
      bg = bg,
      device = dev
    )
    return(invisible(filename))
  }

  if (ext %in% c("png", "tif", "tiff", "jpg", "jpeg")) {
    dev <- switch(
      ext,
      png = if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png",
      tif = if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_tiff else "tiff",
      tiff = if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_tiff else "tiff",
      jpg = if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_jpeg else "jpeg",
      jpeg = if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_jpeg else "jpeg"
    )
    ggplot2::ggsave(
      filename = filename,
      plot = plot,
      width = width,
      height = height,
      dpi = dpi,
      bg = bg,
      device = dev
    )
    return(invisible(filename))
  }

  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    bg = bg
  )
  invisible(filename)
}
