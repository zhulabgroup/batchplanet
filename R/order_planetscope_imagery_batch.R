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
order_planetscope_imagery_batch <- function(dir, df_coordinates, v_site = NULL,
                                            v_year = 2017:(lubridate::year(Sys.Date())),
                                            setting) {
  if (!"site" %in% colnames(df_coordinates)) {
    df_coordinates <- df_coordinates %>%
      mutate(site = "allSite")
  }

  # If site names are not provided, extract unique site names from the location data
  if (is.null(v_site)) {
    v_site <- df_coordinates %>%
      pull(site) %>%
      unique() %>%
      sort()
  }

  # Process orders for each site
  for (siteoi in v_site) {
    df_coordinates_site <- df_coordinates %>%
      filter(site == siteoi) %>%
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
      order_planetscope_imagery_siteyear(dir_site, siteoi, yearoi, bbox, setting)
    }
  }

  invisible(NULL)
}

#' Process Orders for a Single Site and Year
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
order_planetscope_imagery_siteyear <- function(dir_site, siteoi, yearoi, bbox, setting) {
  # Initialize an empty data frame to accumulate order details
  df_order <- data.frame(year = integer(0), month = integer(0), id = character(0), images = integer(0))

  # Loop over each month of the year
  for (monthoi in 1:12) {
    # Get start and end date information for the month
    date_start <- lubridate::floor_date(as.Date(sprintf("%d-%02d-01", yearoi, monthoi)), unit = "month")
    date_end <- lubridate::ceiling_date(date_start, unit = "month") - 1
    doy_start <- as.numeric(format(date_start, "%j"))
    doy_end <- as.numeric(format(date_end, "%j"))

    # Construct a unique order name using site, year, and day-of-year range
    order_name <- str_c(siteoi, yearoi, doy_start, doy_end, sep = "_")

    # Fetch available images from the API based on the date range and bounding box
    # Planet Orders API
    out <- tryCatch(
      {
        images <- search_planetscope_imagery(
          api_key = setting$api_key,
          bbox = bbox,
          date_start = date_start,
          date_end = date_end,
          cloud_lim = setting$cloud_lim,
          ground_control = T,
          quality = "standard",
          item_name = setting$item_name,
          asset = setting$asset
        )
      },
      error = function(e) {
        return(numeric(0))
      }
    )

    # If images are returned, process them into orders and add to the order summary

    if (length(out) > 0) {
      group_id <- ceiling(seq_along(images) / 450)
      image_group <- split(images, group_id)
      for (g in 1:length(image_group)) {
        orderdone <- F
        while (!orderdone) {
          orderdone <- tryCatch(
            {
              order_id <- order_planetscope_imagery(
                api_key = setting$api_key,
                bbox = bbox,
                items = image_group[[g]],
                item_name = setting$item_name,
                product_bundle = setting$product_bundle,
                harmonized = setting$harmonized,
                order_name = order_name,
                mostrecent = 0
              )
              orderdone <- T
            },
            error = function(e) {
              Sys.sleep(60)
              print("Sleep for 60 s.")
              return(F)
            }
          )
        }

        if (!is.null(order_id)) {
          df_order <- df_order %>%
            bind_rows(data.frame(year = yearoi, month = monthoi, id = order_id, images = length(image_group[[g]])))
        }
      }
    }
    message(sprintf("Processed: %s, %d, %02d", siteoi, yearoi, monthoi))
  }

  # Save the accumulated order summary as an RDS file
  if (nrow(df_order) > 0) {
    dir.create(file.path(dir_site, "orders"), showWarnings = FALSE)
    write_rds(df_order, file.path(dir_site, "orders", paste0("order_", yearoi, ".rds")))
  }

  invisible(NULL)
}
