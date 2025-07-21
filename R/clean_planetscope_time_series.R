#' Clean PlanetScope time series for multiple sites and groups
#'
#' Cleans raw time series data for all sites and groups, removing low-quality data and optionally calculating EVI.
#' Low-quality data are defined as:
#' - The sun elevation angle is less than 0 degrees (i.e., night time images).
#' - The reflectance values for any band are less than 0.
#' - The pixel was cloudy, had snow, ice, shadow, haze, or cloud.
#' - The usable data mask had algorithmic confidence in classification less than 80% for the pixel.
#' Results are saved as `.rds` files under the `clean/` subdirectory within `dir`, prefixed `clean_`.
#'
#' @param dir Character. Base directory containing raw time series files (expects a `ts/` subdirectory).
#' @param v_site Character vector, optional. Site identifiers to process; if `NULL`, all sites in `ts/` are included.
#' @param v_group Character vector, optional. Group identifiers to process; if `NULL`, all groups in filenames are included.
#' @param num_cores Integer. Number of parallel workers to use (default: 3).
#' @param calculate_evi Logical. If `TRUE`, computes Enhanced Vegetation Index (`evi`) after cleaning (default: `TRUE`).
#'
#' @return Invisibly returns `NULL` and saves cleaned time series as `.rds` files in the `clean/` subdirectory of `dir`.
#'
#' @examples
#' \dontrun{
#' clean_planetscope_time_series_batch(
#'   dir = "alldata/PSdata/",
#'   v_site = c("HARV", "SJER"),
#'   v_group = c("Acer", "Quercus"),
#'   num_cores = 3,
#'   calculate_evi = TRUE
#' )
#' }
#'
#' @export
clean_planetscope_time_series_batch <- function(dir, v_site = NULL, v_group = NULL, num_cores = 3, calculate_evi = T) {
  dir.create(file.path(dir, "clean"), showWarnings = F)

  v_file <- list.files(file.path(dir, "ts"), recursive = FALSE, full.names = FALSE) %>%
    filter_file_names(v_site, v_group)

  cl <- makeCluster(num_cores, outfile = "")
  registerDoSNOW(cl)

  foreach(
    file = v_file,
    .packages = c("tidyverse", "batchplanet")
  ) %dopar% {
    f_ts <- file.path(dir, "ts", file)
    df_ts <- read_rds(f_ts)

    df_clean <- clean_planetscope_time_series(df_ts, calculate_evi)

    f_clean <- file.path(dir, "clean", file %>% str_replace("ts_", "clean_"))
    write_rds(df_clean, f_clean)
  }
  stopCluster(cl)

  invisible(NULL)
}

#' Clean a single PlanetScope time series
#'
#' Cleans a single time series data frame by removing low-quality data and optionally calculating EVI.
#' Low-quality data are defined as:
#' - The sun elevation angle is less than 0 degrees (i.e., night time images).
#' - The reflectance values for any band are less than 0.
#' - The pixel was cloudy, had snow, ice, shadow, haze, or cloud.
#' - The usable data mask had algorithmic confidence in classification less than 80% for the pixel.
#'
#' @param df_ts Data frame. Raw time series data for a single site/group.
#' @param calculate_evi Logical. If `TRUE`, computes Enhanced Vegetation Index (`evi`) after cleaning (default: `TRUE`).
#'
#' @return Data frame of cleaned time series, with EVI if requested.
#'
#' @examples
#' df_clean <- clean_planetscope_time_series(df_ts = df_ts_example, calculate_evi = TRUE)
#'
#' @export
clean_planetscope_time_series <- function(df_ts, calculate_evi) {
  df_clean <- df_ts %>%
    drop_na() %>%
    mutate(date = as.Date(time)) %>%
    mutate(
      year = format(time, "%Y") %>% as.integer(),
      doy = format(time, "%j") %>% as.integer(),
      hour = format(strptime(time, "%Y-%m-%d %H:%M:%S"), "%H") %>% as.integer()
    ) %>%
    filter(sun_elevation > 0) %>% # remove night time images, but there should not be any in this data product
    filter(red > 0, green > 0, blue > 0) %>%
    filter(clear == 1, snow == 0, shadow == 0, haze_light == 0, haze_heavy == 0, cloud == 0, confidence >= 80) %>%
    group_by(id, lon, lat, date, year, doy) %>%
    summarise(
      blue = mean(blue),
      green = mean(green),
      red = mean(red),
      nir = mean(nir)
    ) %>%
    ungroup()

  if (calculate_evi) {
    df_clean <- df_clean %>%
      mutate(evi = 2.5 * (nir - red) / (nir + 6 * red - 7.5 * blue + 1)) %>%
      filter(evi > 0, evi <= 1)
  }

  return(df_clean)
}
