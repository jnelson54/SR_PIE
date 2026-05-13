#!/usr/bin/env Rscript
# Biplot of fiddler crab isotope data (δ13C vs δ34S) with TEF-corrected sources
# Shows raw consumer data and source values adjusted for trophic enrichment

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
})

# Paths
data_dir <- "plankton_figure_package/PIE_Mixing_Model/fiddler_crabs"
mix_file <- file.path(data_dir, "fiddler_crabs.csv")
source_file <- file.path(data_dir, "PIE_primary_producers.csv")
tef_file <- file.path(data_dir, "TEF_PIE.csv")
out_dir <- file.path(data_dir, "plots")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Load data
mix <- read_csv(mix_file, show_col_types = FALSE)
sources <- read_csv(source_file, show_col_types = FALSE)
tef <- read_csv(tef_file, show_col_types = FALSE)

# Manually extract TEF values (from TEF_PIE.csv structure)
# Project-specific TEF values for PIE
tef_values <- data.frame(
  source = c("benthic microalgae", "pom", "spartina"),
  tef_d13c = c(1.0, 1.0, 1.0),   # δ13C TEF (mean from TEF_PIE.csv)
  tef_d34s = c(0.6, 0.6, 0.6)    # δ34S TEF (mean from TEF_PIE.csv, not 1.2)
)

# Get source means
source_means <- sources %>%
  select(Sources, Meand13C, Meand34S) %>%
  rename(source = Sources, d13c_source = Meand13C, d34s_source = Meand34S)

# Correct sources by adding TEF (consumer = source + TEF)
source_corrected <- source_means %>%
  left_join(tef_values, by = "source") %>%
  mutate(
    d13c_corrected = d13c_source + tef_d13c,
    d34s_corrected = d34s_source + tef_d34s
  )

# Prepare consumer data
consumers <- mix %>%
  select(num, d13C, d34S) %>%
  mutate(type = "Consumer (fiddler crab)")

# Prepare source data (both raw and corrected)
sources_raw <- source_means %>%
  mutate(
    d13C = d13c_source,
    d34S = d34s_source,
    type = "Source (uncorrected)",
    num = NA
  ) %>%
  select(num, d13C, d34S, source, type)

sources_corr <- source_corrected %>%
  mutate(
    d13C = d13c_corrected,
    d34S = d34s_corrected,
    type = "Source (TEF-corrected)",
    num = NA
  ) %>%
  select(num, d13C, d34S, source, type)

# Create biplot
p <- ggplot() +
  # Raw sources
  geom_point(data = sources_raw, 
             aes(d13C, d34S, color = source, shape = type),
             size = 5, fill = NA, stroke = 2) +
  # TEF-corrected sources
  geom_point(data = sources_corr,
             aes(d13C, d34S, color = source, shape = type),
             size = 5, fill = NA, stroke = 2) +
  # Consumers
  geom_point(data = consumers,
             aes(d13C, d34S),
             size = 4, color = "#1B9E77", alpha = 0.7) +
  # Draw arrows from raw to corrected sources (showing TEF adjustment)
  geom_segment(data = source_corrected,
               aes(x = d13c_source, y = d34s_source,
                   xend = d13c_corrected, yend = d34s_corrected,
                   color = source),
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               size = 0.8, alpha = 0.6) +
  
  # Color and shape scales
  scale_color_manual(
    name = "Source",
    values = c("benthic microalgae" = "#1B9E77",
               "pom" = "#D95F02",
               "spartina" = "#7570B3")
  ) +
  scale_shape_manual(
    name = "Type",
    values = c("Consumer (fiddler crab)" = 16,
               "Source (uncorrected)" = 1,
               "Source (TEF-corrected)" = 16)
  ) +
  
  # Labels and theme
  labs(
    title = "Fiddler Crab Isotope Biplot: δ¹³C vs δ³⁴S",
    subtitle = "Sources shown with and without TEF correction; arrows indicate TEF adjustment",
    x = "δ¹³C (‰)",
    y = "δ³⁴S (‰)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 11),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "grey30"),
    panel.grid = element_line(color = "grey90"),
    legend.position = "right"
  )

# Save
pdf_out <- file.path(out_dir, "fiddler_crab_biplot_tef_corrected.pdf")
png_out <- file.path(out_dir, "fiddler_crab_biplot_tef_corrected.png")

ggsave(pdf_out, p, width = 9, height = 7, dpi = 300)
ggsave(png_out, p, width = 9, height = 7, dpi = 300)

cat("Biplot saved to:\n")
cat("  ", normalizePath(pdf_out, winslash = "/"), "\n")
cat("  ", normalizePath(png_out, winslash = "/"), "\n")

# Print summary table
cat("\n=== Isotope Values (‰) ===\n\n")
cat("CONSUMERS (Fiddler crabs):\n")
print(consumers %>% select(num, d13C, d34S) %>% as.data.frame())

cat("\n\nSOURCES - Raw:\n")
print(sources_raw %>% select(source, d13C, d34S) %>% as.data.frame())

cat("\n\nSOURCES - TEF Corrected:\n")
print(source_corrected %>% select(source, d13C = d13c_corrected, d34S = d34s_corrected) %>% as.data.frame())
