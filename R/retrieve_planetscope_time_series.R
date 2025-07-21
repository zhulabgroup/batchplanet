#' Retrieve PlanetScope time series for multiple sites and groups
#'
#' Extracts time series data from PlanetScope imagery for all points in a coordinate data frame, grouped by site and group.
#' For each site and group, reflectance, QA, and metadata are extracted and combined
#' Results are saved as `.rds` files under the `ts/` subdirectory of the specified `dir`, prefixed `ts_`.
#'
#' @param dir Character. Base directory containing raw PlanetScope data (expects a `raw/` subdirectory).
#' @param df_coordinates Data frame with columns `site`, `lon`, `lat`, `id`, and optionally `group`.
#' @param v_site Character vector, optional. Site identifiers to process; if `NULL`, all sites in `df_coordinates` are included.
#' @param v_group Character vector, optional. Group identifiers to process; if `NULL`, all groups in `df_coordinates` are included.
#' @param max_sample Integer. Maximum number of samples per group (default: 2000). This is to avoid processing too many points at once, which can be slow.
#' @param num_cores Integer. Number of parallel workers for processing (default: 12).
#'
#' @return Invisibly returns `NULL` and saves one `.rds` file per site-group to the `ts/` subdirectory in the specified `dir`.
#'
#' @examples
#' \dontrun{
#' retrieve_planetscope_time_series_batch(
#'   dir = "alldata/PSdata/",
#'   df_coordinates = df_coordinates,
#'   v_site = c("HARV", "SJER"),
#'   v_group = c("Acer", "Quercus"),
#'   max_sample = 2000,
#'   num_cores = 12
#' )
#' }
#'
#' @export
retrieve_planetscope_time_series_batch <- function(dir, df_coordinates, v_site = NULL, v_group = NULL, max_sample = 2000, num_cores = 12) {
  # If no specific site is provided, list directories under "raw" for available sites.
  if (is.null(v_site)) {
    v_site <- list.dirs(file.path(dir, "raw"), recursive = FALSE, full.names = FALSE)
  }

  if (!"group" %in% colnames(df_coordinates)) {
    df_coordinates <- df_coordinates %>%
      mutate(group = "allGroup")
  }

  if (is.null(v_group)) {
    v_group <- df_coordinates %>%
      pull(group) %>%
      unique() %>%
      sort()
  }

  # Process each site and group combination sequentially at the outer loop level
  for (siteoi in v_site) {
    for (groupoi in v_group) {
      retrieve_planetscope_time_series_sitegroup(dir, df_coordinates, siteoi, groupoi, max_sample, num_cores)
    }
  }

  invisible(NULL)
}

retrieve_planetscope_time_series_sitegroup <- function(dir, df_coordinates, siteoi, groupoi, max_sample, num_cores) {
  dir_site <- file.path(dir, "raw", siteoi)
  # Count raster files (.tif) for the given site. Skip if none exist.
  file_count <- length(list.files(
    path = dir_site,
    pattern = "\\.tif$", recursive = TRUE
  ))
  if (file_count == 0) {
    return()
  }

  message("Processing: ", siteoi, " - ", groupoi)

  # Filter point data for the current site and group and drop rows missing coordinates
  df_site_group <- df_coordinates %>%
    filter(site == siteoi) %>%
    filter(group == groupoi) %>%
    drop_na(lon, lat) %>%
    sample_n(min(nrow(.), max_sample)) %>%
    arrange(id)

  if (nrow(df_site_group) == 0) {
    return()
  }

  # Extract unique IDs and convert data frame to an sf object
  sf_coordinates <- sf::st_as_sf(df_site_group, coords = c("lon", "lat"), crs = sf::st_crs(4326))

  # Ensure the target directory for time series exists
  dir.create(file.path(dir, "ts"), recursive = TRUE, showWarnings = FALSE)

  # Construct the output filename for the processed time series
  f_ts <- file.path(dir, "ts", paste0("ts_", siteoi, "_", groupoi, ".rds"))

  # If processed file does not exist, process and save the satellite data
  df_ps_full <- retrieve_planetscope_time_series(dir_site, sf_coordinates, num_cores)
  write_rds(df_ps_full, f_ts)
  message("Saved processed data: ", f_ts)
}

#' Retrieve a single set of PlanetScope time series
#'
#' Extracts and combines reflectance, QA, and metadata for a set of spatial points.
#'
#' @param dir_site Character. Path to the site-specific raw data directory.
#' @param sf_coordinates An `sf` object with point coordinates (must have an `id` column).
#' @param num_cores Integer. Number of parallel workers for processing (default: 12).
#'
#' @return Data frame with columns for point ID, coordinates, reflectance bands, QA, and metadata.
#'
#' @examples
#' \dontrun{
#' df_coordinates_example <- df_coordinates %>% filter(site == "SJER", group == "Quercus")
#' df_ts_example <- retrieve_planetscope_time_series(
#'   dir_site = file.path("alldata/PSdata/raw", "SJER"),
#'   sf_coordinates = sf::st_as_sf(df_coordinates_example, coords = c("lon", "lat"), crs = 4326),
#'   num_cores = 12
#' )
#' }
#'
#' @export
retrieve_planetscope_time_series <- function(dir_site, sf_coordinates, num_cores = 12) {
  # Process reflectance data and QA data using specified patterns
  df_ps <- retrieve_raster_data(dir_site, sf_coordinates, type = "sr", num_cores)
  df_ps_qa <- retrieve_raster_data(dir_site, sf_coordinates, type = "udm", num_cores)
  df_ps_meta <- retrieve_metadata(dir_site, num_cores)

  # Extract coordinate information from the spatial points
  df_coordinates <- sf::st_coordinates(sf_coordinates) %>%
    as_tibble() %>%
    mutate(id = sf_coordinates$id) %>%
    rename(lon = X, lat = Y)

  # Combine reflectance, QA, metadata, and coordinate data. Apply scaling to spectral bands.
  df_ps_full <- df_ps %>%
    left_join(df_ps_qa, by = c("id", "f")) %>%
    left_join(df_ps_meta, by = "f") %>%
    left_join(df_coordinates, by = "id") %>%
    mutate(across(c(red, green, blue, nir), ~ .x * 0.0001)) %>%
    select(-f) %>%
    select(id, everything())

  return(df_ps_full)
}

retrieve_raster_data <- function(dir_site, sf_coordinates, type, num_cores) {
  if (type == "sr") {
    pattern <- ".*_SR_.*clip.tif$"
  } else if (type == "udm") {
    pattern <- ".*_udm2_clip.tif$"
  } else {
    stop("Invalid type specified. Use 'sr' for reflectance or 'udm' for QA.")
  }

  files <- list.files(
    path = dir_site, pattern = pattern,
    recursive = TRUE, full.names = TRUE
  ) %>% sort()
  if (length(files) == 0) {
    return(NULL)
  }

  v_id <- sf_coordinates$id

  cl <- makeCluster(num_cores, outfile = "")
  registerDoSNOW(cl)

  # Parallel processing of each file using foreach; each iteration is wrapped in tryCatch
  df_ps <- foreach(
    f = 1:length(files),
    .packages = c("terra", "sf", "tidyverse"),
    .combine = "rbind"
  ) %dopar% {
    tryCatch(
      {
        file <- files[f]
        message("Processing file: ", file)
        ras_ps <- terra::rast(file)

        # Reproject points to match raster CRS
        sf_points_reproj <- sf::st_transform(sf_coordinates, crs = sf::st_crs(ras_ps))

        # Extract raster values at point locations; remove ID column and add file index and point IDs
        df_ps_f <- cbind(terra::extract(ras_ps, sf_points_reproj) %>% select(-ID), f, id = v_id)
        df_ps_f %>% drop_na()
      },
      error = function(e) {
        message("Error processing file ", files[f], ": ", e$message)
        NULL
      }
    )
  }
  stopCluster(cl)

  return(df_ps)
}

retrieve_metadata <- function(dir_site, num_cores) {
  files <- list.files(
    path = dir_site, pattern = "_metadata.json$",
    recursive = TRUE, full.names = TRUE
  ) %>% sort()
  if (length(files) == 0) {
    return(NULL)
  }

  cl <- makeCluster(num_cores, outfile = "")
  registerDoSNOW(cl)

  # Parallel processing of metadata files using foreach with error handling
  df_ps_meta <- foreach(
    f = 1:length(files),
    .packages = c("jsonlite", "tidyverse", "lubridate"),
    .combine = "rbind"
  ) %dopar% {
    tryCatch(
      {
        meta_file <- files[f]
        message("Processing file: ", meta_file)
        metadata <- fromJSON(meta_file)
        data.frame(
          time = metadata$properties$acquired %>% lubridate::as_datetime(),
          sun_elevation = metadata$properties$sun_elevation,
          f
        ) %>% drop_na()
      },
      error = function(e) {
        message("Error processing metadata file ", files[f], ": ", e$message)
        NULL
      }
    )
  }
  stopCluster(cl)

  return(df_ps_meta)
}
