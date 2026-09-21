# Cell-level forest trail and fragmentation metrics
# Run from the repository root.
# This script processes any example site that contains all three cell-level layers:
#   TessellationN.shp, T_CentrelineN.shp, Cell_PatchesN.shp
# The currently supplied Area/Centreline/Patches/Trails files are not substitutes
# for these cell-level inputs.

library(sf)
library(dplyr)

data_dir <- file.path("data", "example")
results_dir <- "results"

cell.metrics <- function(cell, patch, centreline) {
  p.id <- unique(patch$GRID_ID)
  patch.results <- lapply(p.id, function(this.id) {
    patches.in.cell <- patch %>% filter(GRID_ID == this.id)
    this.cell <- cell %>% filter(GRID_ID == this.id)
    if (nrow(patches.in.cell) == 0 || nrow(this.cell) == 0) return(NULL)

    data.frame(
      GRID_ID = this.id,
      cell_size = as.numeric(st_area(this.cell))[1],
      patch_size = as.numeric(patches.in.cell$Shape_Area),
      edge_length = as.numeric(patches.in.cell$Shape_Leng),
      para = as.numeric(patches.in.cell$Shape_Leng / patches.in.cell$Shape_Area),
      patch_count = as.numeric(patches.in.cell$SUM_Count)
    )
  })
  patches <- bind_rows(patch.results)

  c.id <- unique(centreline$GRID_ID)
  centreline.results <- lapply(c.id, function(this.id) {
    trails.in.cell <- centreline %>% filter(GRID_ID == this.id)
    this.cell <- cell %>% filter(GRID_ID == this.id)
    if (nrow(trails.in.cell) == 0 || nrow(this.cell) == 0) return(NULL)
    total.length <- sum(as.numeric(st_length(trails.in.cell)), na.rm = TRUE)
    cell.area <- as.numeric(st_area(this.cell))[1]
    data.frame(
      GRID_ID = this.id,
      trail_length = total.length,
      trail_density = total.length / cell.area * 10000
    )
  })
  centrelines <- bind_rows(centreline.results)
  full_join(patches, centrelines, by = "GRID_ID")
}

cell.junctions <- function(cell, centreline) {
  ids <- unique(cell$GRID_ID)
  junction.results <- lapply(ids, function(this.id) {
    this.cell <- cell %>% filter(GRID_ID == this.id)
    trails.in.cell <- centreline %>% filter(GRID_ID == this.id)
    if (nrow(this.cell) == 0 || nrow(trails.in.cell) == 0) {
      return(data.frame(GRID_ID = this.id, junction_count = 0))
    }

    clipped <- suppressWarnings(st_intersection(st_make_valid(trails.in.cell), st_make_valid(this.cell)))
    clipped_lines <- suppressWarnings(st_collection_extract(clipped, "LINESTRING", warn = FALSE))
    if (nrow(clipped_lines) < 2) return(data.frame(GRID_ID = this.id, junction_count = 0))

    x <- st_make_valid(clipped_lines)
    touch <- st_intersects(x)
    pts <- character(0)
    for (i in seq_along(touch)) {
      js <- touch[[i]]
      js <- js[js > i]
      for (j in js) {
        g <- suppressWarnings(st_intersection(st_geometry(x)[i], st_geometry(x)[j]))
        p <- suppressWarnings(st_collection_extract(g, "POINT"))
        if (length(p) == 0) next
        coords <- st_coordinates(p)
        pts <- c(pts, paste(coords[, "X"], coords[, "Y"], sep = "_"))
      }
    }
    data.frame(GRID_ID = this.id, junction_count = length(unique(pts)))
  })
  bind_rows(junction.results)
}

site_dirs <- list.dirs(data_dir, recursive = FALSE, full.names = TRUE)
site_dirs <- site_dirs[grepl("^site[0-9]+$", basename(site_dirs), ignore.case = TRUE)]

run_site <- function(site_dir) {
  n <- sub("^site0*", "", basename(site_dir), ignore.case = TRUE)
  paths <- c(
    cell = file.path(site_dir, paste0("Tessellation", n, ".shp")),
    centreline = file.path(site_dir, paste0("T_Centreline", n, ".shp")),
    patches = file.path(site_dir, paste0("Cell_Patches", n, ".shp"))
  )
  if (!all(file.exists(paths))) {
    message("Skipping ", basename(site_dir), ": cell-level inputs are not included.")
    return(NULL)
  }

  cell <- st_read(paths[["cell"]], quiet = TRUE)
  centreline <- st_read(paths[["centreline"]], quiet = TRUE)
  patches <- st_read(paths[["patches"]], quiet = TRUE)
  out <- full_join(cell.metrics(cell, patches, centreline), cell.junctions(cell, centreline), by = "GRID_ID")
  out$site <- paste0("site", n)
  out[, c("site", setdiff(names(out), "site"))]
}

results <- Filter(Negate(is.null), lapply(site_dirs, run_site))
if (length(results) == 0) {
  message("No cell-level example datasets were found. Nothing was written. See data/README.md for required inputs.")
} else {
  output <- bind_rows(results)
  dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
  output_file <- file.path(results_dir, "Tessellation_Metrics_example.csv")
  write.csv(output, output_file, row.names = FALSE)
  message("Saved: ", output_file)
}
