#' Batch Order Satellite Imagery
#'
#' Initiates the ordering process for satellite imagery for a set of locations.
#' For each site (optionally provided or extracted from \code{df_locations}) and each year,
#' the function processes orders by calling site- and year-specific routines.
#'
#' @param dir Character. Base directory for saving orders.
#' @param df_locations Data frame containing location records with latitude and longitude.
#' @param v_site Optional character vector. Site names to process. If NULL, all unique sites in \code{df_locations} are used.
#' @param setting List. API settings including API key, item name, asset type, cloud limit, product bundle, and harmonized flag.
#' @param v_year Numeric vector. Years to process (default: 2017 to current year).
#'
#' @return Invisibly returns NULL.
#'
#' @examples
#' \dontrun{
#' df_locations <- read.csv("locations.csv")
#' settings <- list(
#'   api_key = "YOUR_API_KEY", item_name = "PSScene", asset = "ortho_analytic_4b_sr",
#'   cloud_lim = 0.1, product_bundle = "analytic_sr_udm2", harmonized = TRUE
#' )
#' order_satellite_batch("orders_dir/", df_locations, setting = settings)
#' }
#'
#' @export
order_planetscope_batch <- function(dir, df_coordinates, var_site = "site", v_site = NULL,
                                    v_year = 2017:(lubridate::year(Sys.Date())),
                                    setting) {
  if (is.null(var_site)) {
    df_coordinates <- df_coordinates %>%
      mutate(site = "allSite")
    var_site <- "site"
  }

  # If site names are not provided, extract unique site names from the location data
  if (is.null(v_site)) {
    v_site <- df_coordinates %>%
      pull(!!sym(var_site)) %>%
      unique() %>%
      sort()
  }

  # Process orders for each site
  for (siteoi in v_site) {
    df_coordinates_site <- df_coordinates %>%
      filter(!!sym(var_site) == siteoi) %>%
      drop_na(lon, lat)

    if (nrow(df_coordinates_site) == 0) {
      next
    } # Skip processing if no valid locations

    # Define the path for the site's raw data and create an orders directory if needed
    dir_site <- file.path(dir, "raw", siteoi)
    dir.create(file.path(dir_site, "orders"), recursive = TRUE, showWarnings = FALSE)

    # Generate a bounding box for the site based on location coordinates
    bbox <- set_bbox(df_coordinates, siteoi)

    # Process orders for each year
    for (yearoi in v_year) {
      order_planetscope(dir_site, siteoi, yearoi, bbox, setting)
    }
  }

  invisible(NULL)
}

#' Process Orders for a Single Year
#'
#' For a given site and year, this function iterates through all 12 months, fetches available imagery
#' from the API based on the specified date range and bounding box, groups images into orders,
#' and saves an order summary.
#'
#' @param path_sat_site Character. Path to the site's raw data directory.
#' @param siteoi Character. Site name.
#' @param year_download Numeric. Year to process.
#' @param bbox Object. Bounding box for the site.
#' @param setting List. API settings.
#'
#' @return Invisibly returns NULL after saving the order summary.
#' @export
order_planetscope <- function(dir_site, siteoi, yearoi, bbox, setting) {
  # Initialize an empty data frame to accumulate order details
  df_order <- data.frame(year = integer(0), month = integer(0), id = character(0), images = integer(0))

  # Loop over each month of the year
  for (monthoi in 1:12) {
    # Get start and end date information for the month
    date_range <- get_monthly_date_range(yearoi, monthoi)

    # Construct a unique order name using site, year, and day-of-year range
    order_name <- paste(siteoi, yearoi, date_range$start_doy, date_range$end_doy, sep = "_")

    # Fetch available images from the API based on the date range and bounding box
    images <- try_fetch_images(setting, bbox, date_range)

    # If images are returned, process them into orders and add to the order summary
    if (length(images) > 0) {
      df_order <- df_order %>% bind_rows(process_image_groups(images, setting, bbox, order_name, yearoi, monthoi))
    }

    message(sprintf("Processed: %s, %d, %02d", siteoi, yearoi, monthoi))
  }

  # Save the accumulated order summary as an RDS file
  save_order_summary(path_sat_site, year_download, df_order)
}
