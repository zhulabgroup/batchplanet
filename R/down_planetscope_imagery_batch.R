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
down_planetscope_imagery_batch <- function(dir, v_site = NULL, v_year = 2017:(year(Sys.Date())), setting, num_cores = 12) {
  # If no sites are provided, list all available directories under dir/raw/
  if (is.null(v_site)) {
    v_site <- list.dirs(file.path(dir, "raw"), recursive = FALSE, full.names = FALSE)
  }
  
  # Loop over each site
  for (siteoi in v_site) {
    path_sat_site <- file.path(dir, "raw", siteoi)
    
    # Loop over each specified year
    for (year_download in v_year) {
      # Construct the order summary file path and read the file
      order_file <- file.path(path_sat_site, "orders", paste0("order_", year_download, ".rds"))
      if (!file.exists(order_file)) {
        message("Order file not found: ", order_file)
        next
      }
      df_order <- read_rds(order_file)
      
      # If there are orders to process, initiate parallel download
      if (nrow(df_order) > 0) {
        cl <- makeCluster(num_cores, outfile = "")
        registerDoSNOW(cl)
        
        # Set up a progress bar for the download loop
        pb <- txtProgressBar(max = nrow(df_order), style = 3)
        progress <- function(n) setTxtProgressBar(pb, n)
        
        foreach(
          i = 1:nrow(df_order),
          .packages = c("lubridate", "stringr", "httr", "batchplanet"),
          .options.snow = list(progress = progress)
        ) %dopar% {
          # Extract month and order ID for the current order
          month_download <- df_order$month[i]
          order_id <- df_order$id[i]
          
          # Calculate the date range for the given month
          start_year <- year_download
          date_start <- floor_date(as.Date(sprintf("%d-%02d-01", year_download, month_download)), unit = "month")
          date_end <- ceiling_date(as.Date(sprintf("%d-%02d-01", year_download, month_download)), unit = "month") - 1
          start_doy <- as.numeric(format(date_start, "%j"))
          end_doy <- as.numeric(format(date_end, "%j"))
          
          # Create a unique folder name for the order based on site, year, and DOY range
          order_name <- paste(siteoi, start_year, start_doy, end_doy, sep = "_")
          exportfolder <- file.path(path_sat_site, order_name)
          dir.create(exportfolder, recursive = TRUE, showWarnings = FALSE)
          
          # Delay slightly to avoid API rate limits
          Sys.sleep(i * 0.5)
          orderdone <- FALSE
          
          # Retry downloading the order until successful
          while (!orderdone) {
            orderdone <- tryCatch(
              {
                satellite_order_download(order_id, exportfolder, api_key = setting$api_key, order_num = i, overwrite_opt = FALSE)
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
          message(paste(siteoi, year_download, month_download, "downloaded"))
        }
        close(pb)
        stopCluster(cl)
      }
    }
  }
  invisible(NULL)
}