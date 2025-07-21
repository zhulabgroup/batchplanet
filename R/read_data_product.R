#' Read and combine processed data products
#'
#' Reads and combines data files processed with this package(e.g., time series, cleaned remote sensing index, or phenological metrics).
#' Supports reading a subset of data from specified sites and groups.
#' Three data products are supported:
#' - "ts": raw time series data
#' - "clean": cleaned time series data, with optionally calculated Enhanced Vegetation Index (EVI)
#' - "doy": extracted phenological metrics
#'
#' @param dir Character. Base directory containing processed data product files (e.g., the parent directory of "ts/", "clean/", or "doy/").
#' @param v_site Character vector, optional. Site identifiers to include; if `NULL`, all sites are included.
#' @param v_group Character vector, optional. Group identifiers to include; if `NULL`, all groups are included.
#' @param product_type Character. Type of product to read (e.g., "ts", "clean", or "doy"). Default is "clean".
#'
#' @return A data frame combining all matching data product files, with columns for site and group.
#'
#' @examples
#' \dontrun{
#' # Example: Read all cleaned time series for two sites and one group
#' df_clean <- read_data_product(
#'   dir = "alldata/PSdata/",
#'   v_site = c("HARV", "SJER"),
#'   v_group = "Quercus",
#'   product_type = "clean"
#' )
#'
#' # Example: Read all phenological metrics for a site
#' df_doy <- read_data_product(
#'   dir = "alldata/PSdata/",
#'   v_site = "SJER",
#'   product_type = "doy"
#' )
#' }
#'
#' @export
read_data_product <- function(dir, v_site = NULL, v_group = NULL, product_type = "clean") {
  dir_product <- list.files(dir, pattern = product_type, recursive = F, full.names = T)

  v_file <- list.files(dir_product, recursive = FALSE, full.names = FALSE) %>%
    filter_file_names(v_site, v_group)

  ls_df <- list()
  for (file in v_file) {
    # Extract site and group from filenames
    file_parts <- file %>%
      str_remove(".rds") %>%
      str_split("_", simplify = TRUE)
    siteoi <- file_parts[, 2]
    groupoi <- file_parts[, 3]

    f <- file.path(dir_product, file)

    ls_df[[file]] <- read_rds(f) %>%
      mutate(site = siteoi) %>%
      mutate(group = groupoi)
  }
  df <- bind_rows(ls_df)

  return(df)
}

filter_file_names <- function(v_file, v_site = NULL, v_group = NULL) {
  # Extract site and group from filenames
  file_parts <- v_file %>%
    str_remove(".rds") %>%
    str_split("_", simplify = TRUE)
  sites <- file_parts[, 2]
  groups <- file_parts[, 3]

  # If site/group not provided, determine unique values from files
  if (is.null(v_site)) {
    v_site <- unique(sites)
  }
  if (is.null(v_group)) {
    v_group <- unique(groups)
  }

  # Filter files based on v_site and v_group
  keep <- sites %in% v_site & groups %in% v_group
  v_file <- v_file[keep]

  return(v_file)
}
