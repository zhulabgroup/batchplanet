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

order_planetscope_imagery_siteyear <- function(dir_site, siteoi, yearoi, bbox, setting) {
  # Initialize an empty data frame to accumulate order details
  df_order <- data.frame(year = integer(0), month = integer(0), order_name = character(0), order_id = character(0), num_images = integer(0))

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
            bind_rows(data.frame(year = yearoi, month = monthoi, order_name = order_name, order_id = order_id, num_images = length(image_group[[g]])))
        }
      }
    }
    message(sprintf("Processed: %s, %d, %02d", siteoi, yearoi, monthoi))
  }

  # Save the accumulated order summary as an RDS file
  if (nrow(df_order) > 0) {
    dir.create(file.path(dir_site, "orders"), showWarnings = FALSE)
    write_rds(df_order, file.path(dir_site, "orders", str_c("order_", yearoi, ".rds")))
  }

  invisible(NULL)
}

#' Place a Satellite Order Request
#'
#' Submits an order request to the remote sensing API for a given set of image IDs.
#'
#' @param api_key Character. The API key.
#' @param bbox A list with bounding box values.
#' @param items Character vector. A vector of image IDs to order.
#' @param item_name Character. The imagery item type.
#' @param product_bundle Character. The product bundle to order.
#' @param harmonized Logical. Whether to include harmonization (default: FALSE).
#' @param order_name Character. A name for the order.
#' @param mostrecent Numeric. If greater than 0, only the specified number of most recent images are ordered.
#'
#' @return Character. The order ID if successful.
#'
#' @export
order_planetscope_imagery <- function(api_key,
                                      bbox,
                                      items,
                                      item_name,
                                      product_bundle,
                                      harmonized,
                                      order_name,
                                      mostrecent = 0) {
  if (mostrecent > 0) {
    items <- sort(items, decreasing = TRUE)[1:mostrecent]
    message(paste("Selected", mostrecent, "most recent images."))
  }

  tools <- build_order_tools(bbox, harmonized)
  order_json <- build_order_request(order_name, items, item_name, product_bundle, tools)

  url <- "https://api.planet.com/compute/ops/orders/v2"
  req <- httr::POST(url,
    body = order_json,
    httr::content_type_json(),
    username = api_key
  )

  post_content <- httr::content(req)
  if (!is.null(post_content$field$Details[[1]]$message)) {
    message(post_content$field$Details[[1]]$message)
  }

  order_id <- post_content$id
  message(paste("Save the Order ID:", order_id))
  message("You can restart the download with download_planetscope_imagery (order_id, order_name, api_key, order_num)")

  return(order_id)
}

build_order_request <- function(order_name, items, item_name, product_bundle, tools) {
  products <- list(
    list(
      item_ids = items,
      item_type = jsonlite::unbox(item_name),
      product_bundle = jsonlite::unbox(product_bundle)
    )
  )
  order_body <- list(
    name = jsonlite::unbox(order_name),
    products = products,
    tools = tools
  )
  jsonlite::toJSON(order_body, pretty = TRUE)
}

build_order_tools <- function(bbox, harmonized = FALSE) {
  aoi <- build_geojson_from_bbox(bbox)
  clip <- list(aoi = aoi)
  tools <- list(list(clip = clip))
  if (harmonized) {
    harmonize_tool <- list(target_sensor = jsonlite::unbox("Sentinel-2"))
    tools <- list(list(clip = clip), list(harmonize = harmonize_tool))
  }
  return(tools)
}

#' Generate a Bounding Box from Coordinate Data
#'
#' Creates a bounding box for a specified location based on a data frame of coordinates.
#' The function filters the data for the given location, removes any rows with missing longitude
#' or latitude values, and computes the minimum and maximum coordinate values. A buffer is applied
#' to expand the bounding box by a specified distance (in degrees).
#'
#' @param df_coords A data frame containing coordinate data. Must include the columns \code{site},
#'   \code{lon}, and \code{lat}.
#' @param location Character. The location identifier (i.e., the value in the \code{site} column)
#'   for which to generate the bounding box.
#' @param buffer Numeric. A buffer distance (in degrees) to expand the bounding box. Default is 0.0005.
#'
#' @return An object of class \code{bbox} (from the **sf** package) representing the bounding box
#'   for the specified location, or \code{NULL} if no valid coordinates are found.
#'
#' @examples
#' \dontrun{
#' df_coords <- data.frame(
#'   site = c("SiteA", "SiteA", "SiteB"),
#'   lon = c(-77.05, -77.00, -76.95),
#'   lat = c(38.80, 38.85, 38.90)
#' )
#' bbox <- set_bbox(df_coords, "SiteA", buffer = 0.001)
#' }
#'
#' @export
set_bbox <- function(df_coordinates, siteoi, buffer = 0.0005) {
  # Filter the coordinate data for the specified location and remove any rows with missing values.
  df_coordinates_site <- df_coordinates %>%
    filter(site == {{ siteoi }}) %>%
    drop_na(lon, lat)

  # Check if any valid coordinates are found
  if (nrow(df_coordinates_site) == 0) {
    warning("No valid coordinates found for location: ", siteoi)
    return(NULL)
  }

  # Compute the minimum and maximum longitude and latitude, and apply the buffer.
  bbox <- sf::st_bbox(c(
    xmin = min(df_coordinates_site$lon) - buffer,
    xmax = max(df_coordinates_site$lon) + buffer,
    ymin = min(df_coordinates_site$lat) - buffer,
    ymax = max(df_coordinates_site$lat) + buffer
  ))

  return(bbox)
}
