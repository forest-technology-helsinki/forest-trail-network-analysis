# Site-level forest trail and fragmentation metrics
# Run from the repository root.
# Public example data are expected in:
#   data/example/site02/
#   data/example/site04/

library(sf)
library(dplyr)

data_dir <- file.path("data", "example")
results_dir <- "results"

metrics <- function(patch, area, centreline) {
  site.area <- sum(as.numeric(st_area(area)))

  patch$area <- patch$Patch_Size
  patch$edge <- patch$Edge_Leng

  n <- nrow(patch)
  d <- n / (site.area / 10000)
  mean_area <- mean(patch$area, na.rm = TRUE)
  median_area <- median(patch$area, na.rm = TRUE)
  max_area <- max(patch$area, na.rm = TRUE)
  lpi <- max_area / site.area * 100
  pland <- sum(patch$area, na.rm = TRUE) / site.area * 100
  mean_para <- mean(patch$edge / patch$area, na.rm = TRUE)
  median_para <- median(patch$edge / patch$area, na.rm = TRUE)
  edge_sum <- sum(patch$edge, na.rm = TRUE)
  edge_dens <- edge_sum / (site.area / 10000)
  total_length <- sum(as.numeric(st_length(centreline)), na.rm = TRUE)
  trail_density <- total_length / (site.area / 10000)

  data.frame(
    area = site.area,
    num_patches = n,
    patch_density = d,
    mean_patch_size = mean_area,
    median_patch_size = median_area,
    largest_patch_size = max_area,
    largest_patch_index = lpi,
    area_undisturbed = pland,
    mean_para = mean_para,
    median_para = median_para,
    total_edge_length_m = edge_sum,
    edge_density_m_per_ha = edge_dens,
    total_trail_length = total_length,
    trail_density = trail_density
  )
}

sinuosity <- function(centreline) {
  sinuosity_values <- centreline %>%
    rowwise() %>%
    mutate(
      sinuosity = {
        coords <- st_coordinates(geometry)
        if (nrow(coords) >= 3) {
          start_x <- coords[1, "X"]
          start_y <- coords[1, "Y"]
          end_x <- coords[nrow(coords), "X"]
          end_y <- coords[nrow(coords), "Y"]
          path_length <- as.numeric(st_length(geometry))
          euc_distance <- sqrt((end_x - start_x)^2 + (end_y - start_y)^2)
          if (euc_distance >= 0.5) path_length / euc_distance else NA_real_
        } else {
          NA_real_
        }
      }
    ) %>%
    pull(sinuosity)

  data.frame(
    mean_sinuosity = mean(sinuosity_values, na.rm = TRUE),
    median_sinuosity = median(sinuosity_values, na.rm = TRUE)
  )
}

junctions <- function(centreline, area) {
  x <- st_make_valid(centreline)
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

  count <- length(unique(pts))
  site.area <- sum(as.numeric(st_area(area)))

  data.frame(
    junction_count = count,
    junction_density = count / (site.area / 10000)
  )
}

find_example_sites <- function(root) {
  dirs <- list.dirs(root, recursive = FALSE, full.names = TRUE)
  dirs[grepl("^site[0-9]+$", basename(dirs), ignore.case = TRUE)]
}

run_site <- function(site_dir) {
  site_number <- sub("^site0*", "", basename(site_dir), ignore.case = TRUE)

  required <- c(
    patches = file.path(site_dir, paste0("Patches", site_number, ".shp")),
    area = file.path(site_dir, paste0("Area", site_number, ".shp")),
    centreline = file.path(site_dir, paste0("Centreline", site_number, ".shp"))
  )

  missing <- required[!file.exists(required)]
  if (length(missing) > 0) {
    warning("Skipping ", basename(site_dir), "; missing: ", paste(basename(missing), collapse = ", "))
    return(NULL)
  }

  message("Processing ", basename(site_dir), "...")
  patches <- st_read(required[["patches"]], quiet = TRUE)
  area <- st_read(required[["area"]], quiet = TRUE)
  centreline <- st_read(required[["centreline"]], quiet = TRUE)

  out <- cbind(
    metrics(patches, area, centreline),
    sinuosity(centreline),
    junctions(centreline, area)
  )
  out$site <- paste0("site", site_number)
  out[, c("site", setdiff(names(out), "site"))]
}

if (!dir.exists(data_dir)) stop("Example data folder not found: ", data_dir)
site_dirs <- find_example_sites(data_dir)
if (length(site_dirs) == 0) stop("No site folders found under ", data_dir)

results <- lapply(site_dirs, run_site)
results <- Filter(Negate(is.null), results)
if (length(results) == 0) stop("No complete example sites could be processed.")

output <- bind_rows(results)
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
output_file <- file.path(results_dir, "Site_Metrics_example.csv")
write.csv(output, output_file, row.names = FALSE)
message("Saved: ", output_file)
