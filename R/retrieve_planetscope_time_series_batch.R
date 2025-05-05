#' Process Satellite Time Series Data for Spatial Points
#'
#' Processes satellite time series data for spatial points by reading, extracting,
#' and combining reflectance, QA, and metadata information. The resulting combined
#' time series data is saved as an RDS file.
#'
#' @param dir Character. Base directory for satellite data.
#' @param df_coordinates Data frame containing point coordinates. Must include columns: \code{site}, \code{lon}, \code{lat}, \code{id}, and \code{group}.
#' @param v_site Optional character vector. Site identifiers to process. If \code{NULL}, the function
#'        extracts all available sites from \code{dir/raw/}.
#' @param v_group Optional character vector. Group identifiers to process. Defaults to "all".
#' @param max_sample Numeric. Maximum number of samples per group (default: 2000).
#' @param num_cores Numeric. Number of cores for parallel processing (default: detects all available).
#'
#' @return Invisibly returns NULL. Side effect: creates an RDS file containing the processed satellite data.
#'
#' @examples
#' \dontrun{
#' df_coordinates <- read.csv("points.csv")
#' process_satellite_ts("satellite_data/", df_coordinates, v_site = c("Site1", "Site2"))
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

#' Process a Specific Site-Group Combination
#'
#' Processes satellite data for a given site and group. This function:
#'   1. Checks if satellite raster files exist for the site.
#'   2. Filters and samples point data.
#'   3. Converts the sampled data to an sf object.
#'   4. Calls downstream processing to extract and combine reflectance, QA, and metadata.
#'   5. Saves the combined time series as an RDS file.
#'
#' @param dir Character. Base directory for satellite data.
#' @param siteoi Character. Site identifier.
#' @param groupoi Character. Group identifier. If "all", no additional filtering is applied.
#' @param max_sample Numeric. Maximum number of samples to process.
#'
#' @return Invisibly returns NULL.
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

#' Process Satellite Data (Reflectance, QA, and Metadata)
#'
#' Extracts and combines satellite data for a given site.
#' The function processes reflectance data, QA data, and metadata, merges these with point coordinate information,
#' applies scaling to the spectral bands, and returns a combined data frame.
#'
#' @param dir Character. Base directory for satellite data.
#' @param siteoi Character. Site identifier.
#' @param sf_coordinates sf object. Spatial points corresponding to the site's coordinates.
#' @param v_id Character vector. Unique IDs for the spatial points.
#'
#' @return A data frame containing the combined satellite data.
#'
#' @export
retrieve_planetscope_time_series <- function(dir_site, sf_coordinates, num_cores) {
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

#' Process Reflectance or QA Raster Data
#'
#' Extracts data from raster files that match a given pattern (either reflectance or QA)
#' for a specified site.
#'
#' @param dir Character. Base directory for satellite data.
#' @param siteoi Character. Site identifier.
#' @param sf_coordinates sf object. Spatial points object.
#' @param v_id Character vector. Unique IDs for the spatial points.
#' @param pattern Character. Regular expression pattern to match raster files.
#' @param type Character. Type of raster ("sr" for reflectance, "udm" for QA).
#'
#' @return A data frame with extracted raster values.
#'
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

#' Process Metadata JSON Files
#'
#' Reads and processes metadata JSON files associated with a site.
#' Extracts acquisition time and sun elevation, and returns the data as a data frame.
#'
#' @param dir Character. Base directory for satellite data.
#' @param siteoi Character. Site identifier.
#'
#' @return A data frame with metadata information, or \code{NULL} if no metadata files are found.
#'
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
