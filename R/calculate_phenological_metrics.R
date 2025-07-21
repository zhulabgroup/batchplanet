#' Calculate Phenological Metrics for Time Series from multiple sites and groups
#'
#' Reads remote sensing index time series files from `clean/` subdirectory within the specified `dir`,
#' extends the time series for each site, group, and year combination to include the end of the previous year and the beginning of the next year,
#' in order to capture early-year and late-year phenological events,
#' then calls `calculate_phenological_metrics()` to caculate phenological metrics for time series from each site and group.
#' Outputs are saved as `.rds` files under `doy/` subdirectory within specified `dir`, prefixed `doy_`.
#'
#' @param dir Character. Base directory containing remote sensing index files under `clean/` subdirectory.
#' @param v_site Character vector, optional. Site identifiers to process; if `NULL`, all sites are included.
#' @param v_group Character vector, optional. Group identifiers to process; if `NULL`, all groups are included.
#' @param df_thres Data frame of thresholds as from `set_thresholds()`; if `NULL`, uses default thresholds.
#' @param var_index Character. Name of the index column in `df_index` to analyze (default: "evi").
#' @param min_days Numeric. Minimum required number of non-NA data points in one year (default: 80).
#' @param check_seasonality Logical. If `TRUE`, tests for significant seasonal changes before calculating phenological metrics (default: `TRUE`).
#' @param extend_to_previous_year Integer. Day of year to extend backward to, in order to capture early-year phenological events (default: 275).
#' @param extend_to_next_year Integer. Day of year to extend to, in order to capture late-year phenological events (default: 90).
#' @param num_cores Integer. Number of parallel workers for processing (default: 3).
#'
#' @return Invisibly returns `NULL` and saves calculated phenological metrics as `.rds` files into `doy/` subdirectory within specified `dir`.
#'
#' @examples
#' \dontrun{
#' # Batch compute DOY metrics for NEON sites using 4 cores
#' calculate_phenological_metrics_batch(
#'   dir                     = "alldata/PSdata/",
#'   v_site                  = c("Site1","Site2"),
#'   v_group                 = c("Group1","Group2"),
#'   df_thres                = set_thresholds(thres_up = c(0.3,0.5)),
#'   var_index               = "evi",
#'   min_days                = 80,
#'   check_seasonality       = TRUE,
#'   extend_to_previous_year = 275,
#'   extend_to_next_year     = 90,
#'   num_cores               = 3
#' )
#' }
#'
#' @export
calculate_phenological_metrics_batch <- function(dir,
                                                 v_site = NULL,
                                                 v_group = NULL,
                                                 df_thres = NULL,
                                                 var_index = "evi",
                                                 min_days = 80,
                                                 check_seasonality = T,
                                                 extend_to_previous_year = 275,
                                                 extend_to_next_year = 90,
                                                 num_cores = 3) {
  # Use default thresholds if not provided
  if (is.null(df_thres)) {
    df_thres <- set_thresholds()
  }

  # Create output directory for processed DOY data
  dir.create(file.path(dir, "doy"), showWarnings = FALSE)

  v_file <- list.files(file.path(dir, "clean"), recursive = FALSE, full.names = FALSE) %>%
    filter_file_names(v_site, v_group)

  cl <- makeCluster(num_cores, outfile = "")
  registerDoSNOW(cl)

  foreach(
    file = v_file,
    .packages = c("tidyverse", "batchplanet")
  ) %dopar% {
    f_index <- file.path(dir, "clean", file)
    df_index <- read_rds(f_index)

    df_doy <- calculate_phenological_metrics_sitegroup(df_index, df_thres, min_days, check_seasonality, var_index, extend_to_previous_year, extend_to_next_year)

    f_doy <- file.path(dir, "doy", file %>% str_replace("clean_", "doy_"))
    write_rds(df_doy, f_doy)
  }
  stopCluster(cl)

  invisible(NULL)
}

calculate_phenological_metrics_sitegroup <- function(df_index, df_thres, min_days, check_seasonality, var_index = "evi", extend_to_previous_year = 275, extend_to_next_year = 90) {
  v_year <- df_index %>%
    pull(year) %>%
    unique() %>%
    sort()

  ls_df_doy_year <- list()
  for (yearoi in v_year) {
    df_index_year <- df_index %>%
      filter(year == yearoi | year == (yearoi - 1) | year == (yearoi + 1)) %>%
      filter(doy != 366) %>%
      mutate(doy = ifelse(doy >= extend_to_previous_year & year == yearoi - 1, doy - 365, doy)) %>%
      mutate(year = ifelse(doy <= 0 & year == yearoi - 1, year + 1, year)) %>%
      mutate(doy = ifelse(doy <= extend_to_next_year & year == yearoi + 1, doy + 365, doy)) %>%
      mutate(year = ifelse(doy > 365 & year == yearoi + 1, year - 1, year)) %>%
      filter(year == yearoi)

    v_id <- df_index %>%
      pull(id) %>%
      unique() %>%
      sort()

    ls_df_doy_id <- list()
    for (idoi in v_id) {
      message(str_c("Processing time series for ", yearoi, " ", idoi))
      df_index_id <- df_index_year %>%
        filter(id == idoi)

      ls_df_doy_id[[idoi]] <- calculate_phenological_metrics(df_index = df_index_id, df_thres, min_days, check_seasonality, var_index) %>%
        mutate(year = yearoi, id = idoi) %>%
        select(year, id, everything())
    }
    ls_df_doy_year[[yearoi %>% as.character()]] <- bind_rows(ls_df_doy_id)
  }
  df_doy <- bind_rows(ls_df_doy_year)

  return(df_doy)
}

#' Calculate Phenological Metrics for One Time Series
#'
#' This function processes the remote sensing index time series for a single location in one year by gap-filling and smoothing
#'  with Whittaker smoothing, testing for seasonality, and calculating day-of-year (DOY)
#' when the index crosses green-up or green-down threshold(s) specified in `df_thres`.
#' Note that the input index data series is recommended to be extended to include the end of the previous year and the beginning of the next year,
#' in order to capture early-year and late-year phenological events.
#'
#' @param df_index Data frame of remote sensing index time series at one location in one year. Must contain columns `doy` and the index of interest.
#' @param df_thres Data frame containing candidate threshold values, with columns `direction` ("up"/"down") and `threshold` (numeric 0–1).
#' @param min_days Numeric. Minimum required number of non-NA data points in one year (default: 80).
#' @param check_seasonality Logical. If `TRUE`, tests for significant seasonal changes before calculating phenological metrics (default: `TRUE`).
#' @param var_index Character. Name of the index column in `df_index` to analyze (default: "evi").
#'
#' @return A data frame of phenological metrics (DOY) per threshold and direction, or `NULL`
#'   if there are fewer than `min_days` valid data points or if the index does not show a seasonal pattern.
#'
#' @examples
#' \dontrun{
#' # Compute DOY metrics for one smoothed series using custom thresholds
#' df_thres <- set_thresholds(thres_up = c(0.3, 0.5), thres_down = NULL)
#' df_metrics <- calculate_phenological_metrics(
#'   df_index          = df_clean,
#'   df_thres          = df_thres,
#'   min_days          = 250,
#'   check_seasonality = TRUE,
#'   var_index         = "evi"
#' )
#' }
#'
#' @export
calculate_phenological_metrics <- function(df_index, df_thres, min_days, check_seasonality = T, var_index = "evi") {
  # Complete missing days over the extended period (-90 to 455) and apply smoothing
  df_index <- df_index %>%
    arrange(doy) %>%
    mutate(index = !!sym(var_index)) %>%
    complete(doy = seq(min(doy), max(doy), 1), fill = list(index = NA)) %>%
    mutate(index_sm = whittaker_smoothing_filling(x = index, maxgap = 60, lambda = 50, minseg = 2))

  # Count the number of valid (non-NA) smoothed observations
  valid_days <- df_index %>%
    drop_na(index_sm) %>%
    nrow()
  if (valid_days < min_days) {
    return(NULL)
  }

  # Compute a flattened trend indicator using a helper smoothing function
  if (check_seasonality) {
    seasonal <- determine_seasonality(df_index$index_sm, k = 50)
  } else {
    seasonal <- T
  }

  ### green down
  thres_list_down <- df_thres %>%
    filter(direction == "down") %>%
    pull(threshold)
  if (length(thres_list_down) == 0) {
    df_down <- NULL
  } else {
    df_index_max <- df_index %>%
      filter(doy >= 60 & doy <= 300) %>%
      arrange(desc(index_sm), doy) %>%
      slice(1)
    max_index <- df_index_max$index_sm
    start_doy <- df_index_max$doy

    df_index_min <- df_index %>%
      filter(doy >= start_doy) %>%
      arrange(index_sm, desc(doy)) %>%
      slice(1)
    min_index <- df_index_min$index_sm
    end_doy <- df_index_min$doy

    param_ok2 <- (end_doy > start_doy) & seasonal

    if (!param_ok2) {
      greendown_doy <- rep(NA, length(thres_list_down))
      start_doy <- NA
      end_doy <- NA
      print("not typical growth curve")
    } else {
      greendown_thres <- rep(NA, length(thres_list_down))
      for (t in 1:length(thres_list_down)) {
        if (thres_list_down[t] == 1) {
          greendown_thres[t] <- max_index
        } else if (thres_list_down[t] == 0) {
          greendown_thres[t] <- min_index
        } else {
          greendown_thres[t] <- (max_index - min_index) * thres_list_down[t] + min_index
        }
      }
      greendown_thres <- (max_index - min_index) * thres_list_down + min_index

      greendown_doy <- rep(NA, length(greendown_thres))
      for (t in 1:length(greendown_thres)) {
        df_index_doy <- df_index %>%
          filter(
            doy >= start_doy,
            doy <= end_doy
          ) %>%
          filter(index_sm <= greendown_thres[t]) %>%
          arrange(doy) %>%
          slice(1)
        greendown_doy[t] <- df_index_doy$doy
      }
    }
    df_down <- data.frame(start = start_doy, end = end_doy, direction = "down", thres = thres_list_down, doy = greendown_doy)
  }

  ### green up
  thres_list_up <- df_thres %>%
    filter(direction == "up") %>%
    pull(threshold)
  if (length(thres_list_up) == 0) {
    df_up <- NULL
  } else {
    df_index_max <- df_index %>%
      filter(doy >= 60 & doy <= 300) %>%
      arrange(desc(index_sm), doy) %>%
      slice(1)
    max_index <- df_index_max$index_sm
    end_doy <- df_index_max$doy

    df_index_min <- df_index %>%
      filter(doy <= end_doy) %>%
      arrange(index_sm, desc(doy)) %>%
      slice(1)
    min_index <- df_index_min$index_sm
    start_doy <- df_index_min$doy

    param_ok2 <- (end_doy > start_doy) & seasonal

    if (!param_ok2) {
      greenup_doy <- rep(NA, length(thres_list_up))
      start_doy <- NA
      end_doy <- NA
      print("not typical growth curve")
    } else {
      greenup_thres <- rep(NA, length(thres_list_up))
      for (t in 1:length(thres_list_up)) {
        if (thres_list_up[t] == 1) {
          greenup_thres[t] <- max_index
        } else if (thres_list_up[t] == 0) {
          greenup_thres[t] <- min_index
        } else {
          greenup_thres[t] <- (max_index - min_index) * thres_list_up[t] + min_index
        }
      }

      greenup_doy <- rep(NA, length(greenup_thres))
      for (t in 1:length(greenup_thres)) {
        df_index_doy <- df_index %>%
          filter(
            doy >= start_doy,
            doy <= end_doy
          ) %>%
          filter(index_sm >= greenup_thres[t]) %>%
          arrange(doy) %>%
          slice(1)
        greenup_doy[t] <- df_index_doy$doy
      }
    }
    df_up <- data.frame(start = start_doy, end = end_doy, direction = "up", thres = thres_list_up, doy = greenup_doy)
  }

  df_doy <- bind_rows(df_up, df_down)

  return(df_doy)
}

#' Generate Threshold Values for Phenological Events
#'
#' Creates a data frame with candidate threshold values for both increasing
#' and decreasing trends. Optionally, users can apply custom rules to filter the thresholds
#' based on specific conditions (e.g., applying to specific types of data or indices).
#'
#' @param thres_up Numeric vector. Threshold values for increasing trends (default: seq(from = 0, to = 1, by = 0.1) %>% round(1)).
#' @param thres_down Numeric vector. Threshold values for decreasing trends (default: seq(from = 1, to = 0, by = -0.1) %>% round(1)).
#'
#' @return A data frame with two columns:
#'   \item{direction}{A character vector indicating "up" for increasing trends or "down" for decreasing trends.}
#'   \item{threshold}{A numeric vector containing threshold values between 0 and 1.}
#'
#' @examples
#' # Get default thresholds (for increasing and decreasing trends)
#' default_thres <- set_thresholds()
#'
#' # Get thresholds with custom rule
#' custom_thres <- set_thresholds(thres_up = c(0.3,0.5), thres_down = NULL)
#'
#' @export
set_thresholds <- function(thres_up = seq(from = 0, to = 1, by = 0.1) %>% round(1),
                           thres_down = seq(from = 1, to = 0, by = -0.1) %>% round(1)) {
  if (is.null(thres_up)) {
    thres_up <- NA
  }
  if (is.null(thres_down)) {
    thres_down <- NA
  }
  # Combine the upward and downward thresholds into one data frame.
  df_thres <- bind_rows(
    data.frame(direction = "up", threshold = thres_up),
    data.frame(direction = "down", threshold = thres_down)
  ) %>%
    drop_na()

  return(df_thres)
}
