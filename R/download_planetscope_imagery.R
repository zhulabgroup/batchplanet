#' Download Satellite Batch Data
#'
#' Iterates through specified sites and years, reads order information,
#' and downloads satellite imagery for each order.
#'
#' @param dir Character. Base directory containing the raw satellite data.
#' @param v_site Optional character vector. Site names to filter. If NULL, all available
#'               sites in \code{dir/raw/} are used.
#' @param setting List. Contains API settings including the API key (i.e., \code{setting$api_key}).
#' @param v_year Numeric vector. Years for which to download data. Default is from 2017 to the current year.
#' @param download_fn Function. Function to download an individual order. Default is \code{satellite_order_download}.
#'
#' @return Invisibly returns NULL after downloading the order data.
#'
#' @examples
#' \dontrun{
#' settings <- list(api_key = "YOUR_API_KEY")
#' down_satellite_batch("data/", v_site = c("SiteA", "SiteB"), setting = settings, v_year = 2017:2024)
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
    invisible(NULL)
  }
}


download_planetscope_imagery_siteyear <- function(dir_site, siteoi, yearoi, setting, num_cores, overwrite) {
  # Construct the order summary file path and read the file
  order_file <- file.path(dir_site, "orders", str_c("order_", yearoi, ".rds"))
  if (!file.exists(order_file)) {
    message("Order file not found: ", order_file)
    next
  }
  df_order <- read_rds(order_file)

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

#' Download a Satellite Order
#'
#' Retrieves the order details from the remote sensing API and downloads all associated items.
#' The function waits until the order is ready (state = 'success') before downloading.
#'
#' @param order_id Character. The order ID.
#' @param order_name Character. The folder name in which to save the downloaded files.
#' @param api_key Character. The API key.
#' @param order_num Numeric. Identifier for the order used for logging.
#' @param overwrite_opt Logical. Whether to overwrite existing files (default: FALSE).
#'
#' @return Invisibly returns NULL after downloading the files.
#'
#' @export
download_planetscope_imagery <- function(order_id, exportfolder, api_key, overwrite = FALSE) {
  get_content <- wait_for_order_success(order_id, api_key)

  message("Starting download")
  dir.create(exportfolder, showWarnings = FALSE)

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


#' Wait for Order to Succeed
#'
#' Polls the remote sensing API until the order state is 'success', indicating that the order
#' is ready for download.
#'
#' @param order_id Character. The order ID.
#' @param api_key Character. The API key.
#'
#' @return A list containing the order details when the state is 'success'.
#'
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
