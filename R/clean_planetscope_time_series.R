#' Process Index Time Series for a Specific Site
#'
#' Processes index time series data for a given site by iterating over the
#' provided group identifiers. For each group, \code{process_group_index_ts()} is called
#' to load the clean data. The results are returned as a named list, with group names as keys.
#'
#' @param dir Character. The base directory containing satellite data.
#' @param siteoi Character. The site identifier.
#' @param v_group Character vector. A vector of group identifiers.
#' @param parallel Logical. Whether to load the groups in parallel (default: FALSE).
#'
#' @return A named list where each element corresponds to the index time series data for a group.
#'   Groups with no available data will have a value of \code{NULL}.
#'
#' @examples
#' \dontrun{
#' result_list <- process_site_index_ts("test_data", "TestSite", c("GroupA", "GroupB"))
#' # Access data for GroupA:
#' df_groupA <- result_list[["GroupA"]]
#' }
#'
#' @export
clean_planetscope_time_series_batch <- function(dir, v_site = NULL, v_group = NULL, num_cores, calculate_evi = T) {
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

    # Warn and return NULL if the file is missing or contains no data
    if (is.null(df_ts) || nrow(df_ts) == 0) {
      warning("Skipping: No data found for ", f_ts)
      return(NULL)
    } else {
      df_clean <- clean_planetscope_time_series(df_ts, calculate_evi)
      f_clean <- file.path(dir, "clean", file %>% str_replace("ts_", "clean_"))
      write_rds(df_clean, f_clean)
    }
  }
  stopCluster(cl)
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
