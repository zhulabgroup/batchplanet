#' Search for available PlanetScope imagery
#'
#' Queries the Planet API for imagery overlapping the specified bounding box and date range, then filters results by cloud cover, ground control, and quality.
#' Returns only IDs for which you have download permission.
#'
#' @param api_key Character. Your Planet API key.
#' @param bbox Named numeric list with `xmin`, `ymin`, `xmax`, `ymax` defining the search area.
#' @param date_start Character. Start date ("YYYY-MM-DD") for the search (inclusive).
#' @param date_end Character. End date ("YYYY-MM-DD") for the search (inclusive).
#' @param item_name Character. Planet item type to search (default: `"PSScene"`).
#' @param asset Character. Asset type to filter permissions (default: `"ortho_analytic_4b_sr"`).
#' @param cloud_lim Numeric. Maximum allowed cloud cover fraction (0–1, default: 0.1).
#' @param ground_control Logical. If `TRUE`, require ground control metadata (default: `TRUE`).
#' @param quality Character. Quality category filter (default: `"standard"`).
#'
#' @return Character vector of image IDs with download permission. Returns `NULL` if none are found.
#'
#' @examples
#' \dontrun{
#' my_bbox <- list(xmin = -77.05, ymin = 38.80, xmax = -76.90, ymax = 39.00)
#' ids <- search_planetscope_imagery(
#'   api_key = set_api_key(),
#'   bbox = my_bbox,
#'   date_start = "2023-06-01",
#'   date_end = "2023-06-30",
#'   cloud_lim = 1,
#'   ground_control = TRUE,
#'   quality = "standard"
#' )
#' }
#'
#' @export
search_planetscope_imagery <- function(api_key,
                                       bbox,
                                       date_end = NULL,
                                       date_start = NULL,
                                       item_name = "PSScene",
                                       asset = "ortho_analytic_4b_sr",
                                       cloud_lim = 0.1,
                                       ground_control = TRUE,
                                       quality = "standard") {
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

  return(images)
}

build_geojson_from_bbox <- function(bbox) {
  list(
    type = jsonlite::unbox("Polygon"),
    coordinates = list(list(
      c(bbox$xmin, bbox$ymin),
      c(bbox$xmin, bbox$ymax),
      c(bbox$xmax, bbox$ymax),
      c(bbox$xmax, bbox$ymin),
      c(bbox$xmin, bbox$ymin)
    ))
  )
}

build_geometry_filter <- function(bbox) {
  list(
    type = jsonlite::unbox("GeometryFilter"),
    field_name = jsonlite::unbox("geometry"),
    config = build_geojson_from_bbox(bbox)
  )
}

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

build_cloud_cover_filter <- function(cloud_lim = 0.1) {
  list(
    type = jsonlite::unbox("RangeFilter"),
    field_name = jsonlite::unbox("cloud_cover"),
    config = list(
      lte = jsonlite::unbox(cloud_lim)
    )
  )
}

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

combine_filters <- function(date_filter, cloud_filter, gq_filter, geometry_filter) {
  list(
    type = jsonlite::unbox("AndFilter"),
    config = list(date_filter, cloud_filter, gq_filter, geometry_filter)
  )
}

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
    message(paste("Found", nrow(permissions), "suitable", asset, "images with DOWNLOAD permissions."))

    if (nrow(permissions) > 0) {
      return(permissions$id)
    } else {
      message(paste("You DO NOT have DOWNLOAD permissions for these images. You have", toupper(unique(permissions$permission)), "permission"))
    }
  }
}
