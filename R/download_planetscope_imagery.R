#' Download PlanetScope imagery for multiple sites and years
#'
#' Downloads all ordered PlanetScope imagery for specified sites and years. The function will wait until the order is successfully processed by Planet API, and then download the files.
#'
#' @param dir Character. Base directory where the raw satellite data will be stored, expects a `raw/` subdirectory.
#' @param v_site Character vector, optional. Site names to filter; if `NULL`, all sites are used.
#' @param setting List. Contains API settings including the API key (i.e., `setting$api_key`).
#' @param v_year Numeric vector. Years for which to download data. Default is from 2017 to the current year.
#' @param num_cores Integer. Number of parallel workers to use (default: 12).
#' @param overwrite Logical. If `TRUE`, existing files will be overwritten (default: `FALSE`).
#'
#' @return Invisibly returns `NULL` and saves downloaded imagery to the `raw/` subdirectory of the specified `dir`.
#'
#' @examples
#' \dontrun{
#' download_planetscope_imagery_batch(
#'   dir = "alldata/PSdata/",
#'   v_site = c("HARV", "SJER"),
#'   v_year = 2025,
#'   setting = setting,
#'   num_cores = 3,
#'   overwrite = FALSE
#' )
#' }
#'
#' @export
download_planetscope_imagery_batch <- function(dir, v_site = NULL, v_year = 2017:(year(Sys.Date())), setting, num_cores = 12, overwrite = F) {
  # If no sites are provided, list all available directories under dir/raw/
  if (is.null(v_site)) {
    v_site <- list.dirs(file.path(dir, "raw"), recursive = FALSE, full.names = FALSE)
  }

  # Loop over each site
  for (siteoi in v_site) {
    dir_site <- file.path(dir, "raw", siteoi)

    # Loop over each specified year
    for (yearoi in v_year) {
      download_planetscope_imagery_siteyear(dir_site, siteoi, yearoi, setting, num_cores, overwrite)
    }
  }
  invisible(NULL)
}

download_planetscope_imagery_siteyear <- function(dir_site, siteoi, yearoi, setting, num_cores, overwrite) {
  # Construct the order summary file path and read the file
  order_file <- file.path(dir_site, "orders", str_c("order_", yearoi, ".rds"))
  if (!file.exists(order_file)) {
    message("Order file not found: ", order_file)
    df_order <- data.frame()
  } else {
    df_order <- read_rds(order_file)
  }

  # If there are orders to process, initiate parallel download
  if (nrow(df_order) > 0) {
    cl <- makeCluster(num_cores, outfile = "")
    registerDoSNOW(cl)

    foreach(
      i = 1:nrow(df_order),
      .packages = c("lubridate", "stringr", "httr", "batchplanet")
    ) %dopar% {
      # Extract month and order ID for the current order
      monthoi <- df_order$month[i]
      order_name <- df_order$order_name[i]
      order_id <- df_order$order_id[i]

      exportfolder <- file.path(dir_site, order_name)
      dir.create(exportfolder, recursive = TRUE, showWarnings = FALSE)

      # Delay slightly to avoid API rate limits
      Sys.sleep(i * 0.5)
      orderdone <- FALSE

      # Retry downloading the order until successful
      while (!orderdone) {
        orderdone <- tryCatch(
          {
            download_planetscope_imagery(order_id, exportfolder, api_key = setting$api_key, overwrite)
            TRUE
          },
          error = function(e) {
            message("Error downloading order ", order_id, ": ", e$message, "\nRetrying in 10s...")
            Sys.sleep(10)
            FALSE
          }
        )
      }

      # Log progress for this order
      message(str_c(order_name, " download complete"))
    }
    stopCluster(cl)
  }
}

#' Download a PlanetScope order
#'
#' Downloads all files for a given PlanetScope order ID, saving them to the specified folder. The function will wait until the order is successfully processed by Planet API.
#'
#' @param order_id Character. The PlanetScope order ID.
#' @param exportfolder Character. Directory in which to save downloaded files (created if needed).
#' @param api_key Character. Your Planet API key.
#' @param overwrite Logical. If `TRUE`, existing files will be overwritten (default: `FALSE`).
#'
#' @return Invisibly returns `NULL` and writes all imagery files to the specified `exportfolder`.
#'
#' @examples
#' \dontrun{
#' download_planetscope_imagery(
#'   order_id = "abc123-order-id",
#'   exportfolder = "data/raw/SJER/SJER_202405_121_151",
#'   api_key = set_api_key(),
#'   overwrite = TRUE
#' )
#' }
#'
#' @export
download_planetscope_imagery <- function(order_id, exportfolder, api_key, overwrite = FALSE) {
  get_content <- wait_for_order_success(order_id, api_key)

  message("Starting download")
  dir.create(exportfolder, showWarnings = FALSE, recursive = TRUE)

  for (i in seq_along(get_content$`_links`$results)) {
    # message(str_c("Order ", order_num, ", Download: ", round(100 * (i / length(get_content$`_links`$results)), 2), "%"))
    name <- get_content$`_links`$results[[i]]$name
    findslash <- gregexpr("/", name)
    startchar <- findslash[[1]][length(findslash[[1]])] + 1
    filename <- substr(name, startchar, nchar(name))

    download_url <- get_content$`_links`$results[[i]]$location

    try({
      httr::RETRY("GET",
        url = download_url,
        username = api_key,
        httr::write_disk(path = file.path(exportfolder, filename), overwrite)
      )
      message(paste("Downloaded file ", filename))
    })
  }
}

wait_for_order_success <- function(order_id, api_key) {
  url2 <- paste0("https://api.planet.com/compute/ops/orders/v2/", order_id)
  get_order <- httr::GET(url = url2, username = api_key)
  get_content <- httr::content(get_order)

  while (get_content$state != "success") {
    message("Order still being processed, trying again in 60 seconds...")
    message(get_content$state)
    Sys.sleep(60)
    get_order <- httr::GET(url = url2, username = api_key)
    get_content <- httr::content(get_order)
  }

  get_content
}
