#' Process DOY Data from Satellite Time Series
#'
#' This function processes day-of-year (DOY) data from index time series files
#' stored in the specified directory. For each site and group combination, it reads
#' the corresponding index data, computes DOY values based on threshold crossings,
#' and saves the processed DOY data as an RDS file.
#'
#' @param dir Character. Directory containing index data (default: "alldata/PSdata/").
#' @param v_site Optional character vector. Site identifiers to process.
#' @param v_group Optional character vector. Group identifiers to process.
#' @param v_year Optional numeric vector. Years to process.
#' @param v_id Optional character vector. IDs to process. If NULL, IDs are extracted from the data.
#' @param df_thres Optional data frame of threshold values. If NULL, defaults to \code{set_thres()}.
#' @param min_days Numeric. Minimum number of valid data points required for processing (default: 300).
#' @param parallel Logical. Whether to use parallel processing (default: FALSE).
#'
#' @return Invisibly saves processed DOY data as RDS files and returns \code{NULL}.
#'
#' @examples
#' \dontrun{
#' proc_doy(dir = "alldata/PSdata/", v_site = c("Site1", "Site2"), v_group = c("GroupA", "GroupB"))
#' }
#'
#' @export
calculate_phenological_metrics_batch <- function(dir,
                                                 v_site = NULL,
                                                 v_group = NULL,
                                                 df_thres = NULL,
                                                 min_days = 300,
                                                 check_seaonality = T,
                                                 num_cores = 3) {
  # Use default thresholds if not provided
  if (is.null(df_thres)) {
    df_thres <- set_thresholds()
  }

  # Create output directory for processed DOY data
  doy_dir <- file.path(dir, "doy")
  dir.create(doy_dir, showWarnings = FALSE, recursive = TRUE)

  if (is.null(v_site)) {
    v_site <- "dummy"
  }
  if (is.null(v_group)) {
    v_group <- "dummy"
  }
  v_file <- expand_grid(site = v_site, group = v_group) %>%
    mutate(file = str_c("_", site, "_", group)) %>%
    pull(file) %>%
    str_remove_all("_dummy")

  cl <- makeCluster(num_cores, outfile = "")
  registerDoSNOW(cl)

  foreach(
    file = v_file,
    .packages = c("tidyverse", "batchplanet")
  ) %dopar% {
    f_evi <- list.files(dir, pattern = str_c("evi", file, ".rds"), recursive = T, full.names = T)
    f_doy <- str_c(dir, "doy/doy", file, ".rds")
    if (length(f_evi) == 1) {
      df_evi <- read_rds(f_evi)

      v_year <- df_evi %>%
        pull(year) %>%
        unique() %>%
        sort()

      ls_df_doy_year <- list()
      for (yearoi in v_year) {
        df_evi_year <- df_evi %>%
          filter(year == yearoi | year == (yearoi - 1) | year == (yearoi + 1)) %>%
          filter(doy != 366) %>%
          mutate(doy = ifelse(doy > 274 & year == yearoi - 1, doy - 365, doy)) %>%
          mutate(year = ifelse(doy <= 0 & year == yearoi - 1, year + 1, year)) %>%
          mutate(doy = ifelse(doy < 91 & year == yearoi + 1, doy + 365, doy)) %>%
          mutate(year = ifelse(doy > 365 & year == yearoi + 1, year - 1, year)) %>%
          filter(year == yearoi)

        v_id <- df_evi %>%
          pull(id) %>%
          unique() %>%
          sort()

        ls_df_doy_id <- list()
        for (idoi in v_id) {
          print(str_c(yearoi, " ", idoi))
          df_evi_id <- df_evi_year %>%
            filter(id == idoi)

          ls_df_doy_id[[idoi]] <- proc_doy_1ts(df_ts_ind = df_evi_id, df_thres = df_thres, min_days = min_days) %>%
            mutate(year = yearoi, id = idoi) %>%
            select(year, id, everything())
        }
        ls_df_doy_year[[yearoi %>% as.character()]] <- bind_rows(ls_df_doy_id)
      }
      df_doy <- bind_rows(ls_df_doy_year)
      write_rds(df_doy, f_doy)
    }
  }
  stopCluster(cl)
}


#' Generate Threshold Values for Significant Events
#'
#' Creates a data frame with candidate threshold values for both increasing
#' and decreasing trends. Optionally, users can apply custom rules to filter the thresholds
#' based on specific conditions (e.g., applying to specific types of data or indices).
#'
#' @param custom_rule A list specifying custom threshold rules. If \code{NULL} (default),
#'   a default set of thresholds is returned.
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
#' custom_thres <- set_thresholds(custom_rule = list(up = 0.7, down = 0.3))
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
