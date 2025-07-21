#' Clean PlanetScope Time Series for multiple sites and groups
#' 
#' @param dir: Character. Base directory containing raw time series files under `ts/` subdirectory.
#' @param v_site: Character vector, optional. Site identifiers to process; if NULL, defaults to all sites in `ts/`.
#' @param v_group: Character vector, optional. Group identifiers to process; if NULL, defaults to all groups in filenames.
#' @param num_cores: Integer. Number of parallel workers to use (default: 3).
#' @param calculate_evi: Logical. If TRUE, computes Enhanced Vegetation Index (`evi`) after cleaning (default: TRUE).
#' 
#' @return: Invisibly returns NULL and saves cleaned time series as `.rds` files under `clean/` subdirectory within specified `dir`, prefixed `clean_`.
#' 
#' @examples
#' \dontrun{
#' # Clean all time series for NEON sites A and B using 4 cores
#' clean_planetscope_time_series_batch(
#'   dir           = "alldata/PlanetScope/",
#'   v_site        = c("Site1", "Site2"),
#'   v_group       = c("Group1", "Group2"),
#'   num_cores     = 3,
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
