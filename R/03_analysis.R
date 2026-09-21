# Analysis entry point for the public example data
# Run from the repository root.
# With only Sites 2 and 4, this script provides descriptive summaries and plots.
# The original 15-site inferential analyses should not be interpreted from a two-site example.

library(tidyverse)
library(ggplot2)
library(flextable)
library(officer)

results_dir <- "results"
site_file <- file.path(results_dir, "Site_Metrics_example.csv")
cell_file <- file.path(results_dir, "Tessellation_Metrics_example.csv")

if (!file.exists(site_file)) {
  stop("Missing ", site_file, ". Run R/01_site_metrics.R first.")
}

site.raw <- read.csv(site_file)
cell.raw <- if (file.exists(cell_file)) read.csv(cell_file) else NULL

print(site.raw)

# Descriptive site-level plots. With two example sites these are demonstrations,
# not a basis for the original study's regression inference.
site_plots <- list(
  trail_density_vs_patch_size = ggplot(site.raw, aes(trail_density, mean_patch_size, label = site)) +
    geom_point(size = 3) + geom_text(nudge_y = 0.03 * max(site.raw$mean_patch_size, na.rm = TRUE)) +
    labs(title = "Trail Density vs Mean Patch Size", x = "Trail Density (m/ha)", y = "Mean Patch Size (m²)") + theme_classic(),
  trail_density_vs_patch_density = ggplot(site.raw, aes(trail_density, patch_density, label = site)) +
    geom_point(size = 3) + geom_text(nudge_y = 0.03 * max(site.raw$patch_density, na.rm = TRUE)) +
    labs(title = "Trail Density vs Patch Density", x = "Trail Density (m/ha)", y = "Patch Density (ha⁻¹)") + theme_classic(),
  junction_density_vs_patch_density = ggplot(site.raw, aes(junction_density, patch_density, label = site)) +
    geom_point(size = 3) + geom_text(nudge_y = 0.03 * max(site.raw$patch_density, na.rm = TRUE)) +
    labs(title = "Junction Density vs Patch Density", x = "Junction Density (ha⁻¹)", y = "Patch Density (ha⁻¹)") + theme_classic(),
  median_para_vs_trail_density = ggplot(site.raw, aes(median_para, trail_density, label = site)) +
    geom_point(size = 3) + geom_text(nudge_y = 0.03 * max(site.raw$trail_density, na.rm = TRUE)) +
    labs(title = "Median PARA vs Trail Density", x = "PARA (m⁻¹)", y = "Trail Density (m/ha)") + theme_classic()
)

for (p in site_plots) print(p)

# Site results table
selected <- site.raw %>%
  select(site, area, num_patches, patch_density, mean_patch_size, median_patch_size,
         largest_patch_size, largest_patch_index, area_undisturbed, mean_para,
         median_para, total_edge_length_m, edge_density_m_per_ha, total_trail_length,
         trail_density, mean_sinuosity, median_sinuosity, junction_count, junction_density)

site.table <- flextable(selected) %>% autofit()
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
print(read_docx() %>% body_add_flextable(site.table),
      target = file.path(results_dir, "Site_Results_Table_example.docx"))

# Cell-level examples are optional and run only when the required inputs exist.
if (!is.null(cell.raw)) {
  print(
    ggplot(cell.raw, aes(trail_density, para)) +
      geom_point() +
      labs(title = "Cell-level Trail Density vs PARA", x = "Trail Density (m/ha)", y = "PARA (m⁻¹)") +
      theme_classic()
  )
} else {
  message("Cell-level results not found; skipping cell-level analysis.")
}

message("Example analysis complete. Outputs are in: ", results_dir)
