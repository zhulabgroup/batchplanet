#' Search for Satellite Imagery
#'
#' Queries a remote sensing API for imagery overlapping a given bounding box and meeting
#' specified filtering criteria. Returns a vector of image IDs for which the user has
#' download permission.
#'
#' @param bbox List. A list with \code{xmin}, \code{ymin}, \code{xmax}, and \code{ymax}.
#' @param date_end Character. End date ("YYYY-MM-DD") for the search.
#' @param date_start Character. Start date ("YYYY-MM-DD") for the search.
#' @param cloud_lim Numeric. Maximum allowed cloud cover (default: 0.1).
#' @param ground_control Logical. Whether ground control is required (default: TRUE).
#' @param quality Character. Quality filter (default: "standard").
#' @param item_name Character. Imagery item type (default: "PSScene").
#' @param asset Character. Asset type (default: "ortho_analytic_4b_sr").
#' @param api_key Character. The API key.
#' @param list_dates Optional vector of dates for filtering.
#'
#' @return A character vector of image IDs with download permission, or \code{NULL} if none found.
#'
#' @examples
#' \dontrun{
#' bbox <- list(xmin = -77.05, ymin = 38.80, xmax = -76.90, ymax = 39.00)
#' image_ids <- search_satellite_imagery(
#'   bbox = bbox,
#'   date_start = "2023-06-01",
#'   date_end = "2023-06-30",
#'   api_key = "YOUR_API_KEY"
#' )
#' }
#'
#' @export
search_planetscope_imagery <- function(bbox,
                                       date_end = NULL,
                                       date_start = NULL,
                                       cloud_lim = 0.1,
                                       ground_control = TRUE,
                                       quality = "standard",
                                       item_name = "PSScene",
                                       asset = "ortho_analytic_4b_sr",
                                       api_key = "test") {
  message(str_c("Searching for satellite imagery from ", date_start, " to ", date_end))
  # Build filters
  geometry_filter <- build_geometry_filter(bbox)
  date_filter <- build_date_range_filter(date_start, date_end)
  cloud_filter <- build_cloud_cover_filter(cloud_lim)
  gq_filter <- build_gq_filter(ground_control, quality)
  filter_configs <- combine_filters(date_filter, cloud_filter, gq_filter, geometry_filter)

  # Build the search request body
  search_request <- list(
    item_types = item_name,
    filter = filter_configs
  )
  body_json <- jsonlite::toJSON(search_request, pretty = TRUE)

  # Submit the POST request to the API endpoint
  url <- "https://api.planet.com/data/v1/quick-search"
  req <- httr::POST(url,
    body = body_json,
    httr::content_type_json(),
    httr::authenticate(api_key, "")
  )

  res <- jsonlite::fromJSON(httr::content(req, as = "text", encoding = "UTF-8"))

  # Fetch download permissions from all pages of results
  images <- extract_imagery_id(res, api_key, asset)

  invisible(images)
}



### Helper Functions for Searching

#' Build GeoJSON Geometry from Bounding Box
#'
#' Constructs a GeoJSON Polygon from a bounding box.
#'
#' @param bbox A list with elements \code{xmin}, \code{ymin}, \code{xmax}, and \code{ymax}.
#' @return A list representing the GeoJSON polygon.
#'
#' @examples
#' bbox <- list(xmin = -77.05, ymin = 38.80, xmax = -76.90, ymax = 39.00)
#' geojson <- build_geojson_from_bbox(bbox)
#'
build_geometry_filter <- function(bbox) {
  list(
    type = jsonlite::unbox("GeometryFilter"),
    field_name = jsonlite::unbox("geometry"),
    config = list(
      type = jsonlite::unbox("Polygon"),
      coordinates = list(list(
        c(bbox$xmin, bbox$ymin),
        c(bbox$xmin, bbox$ymax),
        c(bbox$xmax, bbox$ymax),
        c(bbox$xmax, bbox$ymin),
        c(bbox$xmin, bbox$ymin)
      ))
    )
  )
}

#' Build Date Range Filter
#'
#' Constructs a date range filter for the search request.
#'
#' @param date_start Character. Start date in "YYYY-MM-DD" format.
#' @param date_end Character. End date in "YYYY-MM-DD" format.
#' @param list_dates Optional vector of dates to refine the filter.
#'
#' @return A list representing the date range filter.
#'
build_date_range_filter <- function(date_start, date_end) {
  dategte <- paste0(date_start, "T00:00:00.000Z")
  datelte <- paste0(date_end, "T00:00:00.000Z")
  list(
    type = jsonlite::unbox("DateRangeFilter"),
    field_name = jsonlite::unbox("acquired"),
    config = list(
      gte = jsonlite::unbox(dategte),
      lte = jsonlite::unbox(datelte)
    )
  )
}

#' Build Cloud Cover Filter
#'
#' Constructs a cloud cover filter.
#'
#' @param cloud_lim Numeric. Maximum allowed cloud cover (0-1).
#'
#' @return A list representing the cloud cover filter.
#'
build_cloud_cover_filter <- function(cloud_lim = 0.1) {
  list(
    type = jsonlite::unbox("RangeFilter"),
    field_name = jsonlite::unbox("cloud_cover"),
    config = list(
      lte = jsonlite::unbox(cloud_lim)
    )
  )
}

#' Build Ground Control and Quality Filter
#'
#' Constructs a filter for ground control and quality.
#'
#' @param ground_control Logical. Whether ground control is required.
#' @param quality Character. Quality filter (default: "standard").
#'
#' @return A list representing the ground control and quality filter.
#'
build_gq_filter <- function(ground_control = TRUE, quality = "standard") {
  list(
    type = jsonlite::unbox("AndFilter"),
    config = list(
      list(
        field_name = jsonlite::unbox("ground_control"),
        type = jsonlite::unbox("StringInFilter"),
        config = list(jsonlite::unbox(tolower(ground_control)))
      ),
      list(
        field_name = jsonlite::unbox("quality_category"),
        type = jsonlite::unbox("StringInFilter"),
        config = list(jsonlite::unbox(quality))
      )
    )
  )
}

#' Combine Search Filters
#'
#' Combines date range, cloud cover, ground control/quality, and geometry filters.
#'
#' @param date_filter List. Date range filter.
#' @param cloud_filter List. Cloud cover filter.
#' @param gq_filter List. Ground control and quality filter.
#' @param geometry_filter List. Geometry filter.
#'
#' @return A list representing the combined filter configuration.
#'
combine_filters <- function(date_filter, cloud_filter, gq_filter, geometry_filter) {
  list(
    type = jsonlite::unbox("AndFilter"),
    config = list(date_filter, cloud_filter, gq_filter, geometry_filter)
  )
}

#' Extract Download Permissions from API Response
#'
#' Extracts the download permissions for the specified asset from the API response.
#'
#' @param res List. The API response parsed as a list.
#' @param asset Character. The asset type to filter (e.g., "ortho_analytic_4b_sr").
#'
#' @return A data frame containing image IDs with download permission.
#'
extract_permissions <- function(res, asset) {
  permissions <- do.call(rbind, lapply(seq_along(res$features$`_permissions`), function(i) {
    perms <- stringr::str_split(res$features$`_permissions`[[i]], ":", simplify = TRUE)
    data.frame(
      id = res$features$id[i],
      asset = gsub("assets.", "", perms[, 1]),
      permission = perms[, 2],
      stringsAsFactors = FALSE
    )
  }))
  resDFid <- permissions[permissions$asset == asset, ]
  resDFid[resDFid$permission == "download", ]
}

#' Fetch All Permissions Across Pagination
#'
#' Retrieves all pages of search results and extracts download permissions.
#'
#' @param initial_res List. The initial API response.
#' @param api_key Character. The API key.
#' @param asset Character. The asset type.
#'
#' @return A data frame containing permissions from all pages.
#'
extract_imagery_id <- function(initial_res, api_key, asset) {
  permissions <- extract_permissions(initial_res, asset)
  res <- initial_res
  while (!is.null(res$`_links`$`_next`)) {
    next_req <- httr::GET(
      res$`_links`$`_next`,
      httr::content_type_json(),
      httr::authenticate(api_key, "")
    )
    res <- jsonlite::fromJSON(httr::content(next_req, as = "text", encoding = "UTF-8"))
    if (!is.null(unlist(res$features))) {
      permissions <- rbind(permissions, extract_permissions(res, asset))
    }
  }
  permissions[!is.na(permissions$id), ]

  if (unique(permissions$permission) == "download") {
    message(paste("Found", nrow(permissions), "suitable", item_name, asset, "images with DOWNLOAD permissions."))

    if (nrow(permissions) > 0) {
      return(permissions$id)
    } else {
      message(paste("You DO NOT have DOWNLOAD permissions for these images. You have", toupper(unique(permissions$permission)), "permission"))
    }
  }
}
