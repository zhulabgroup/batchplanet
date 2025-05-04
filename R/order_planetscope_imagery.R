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
                                      harmonized = FALSE,
                                      order_name = exportfolder,
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


#' Build Order Request Body
#'
#' Constructs the JSON request body for placing an order.
#'
#' @param order_name Character. The name for the order.
#' @param items Character vector. A vector of image IDs.
#' @param item_name Character. The type of imagery item.
#' @param product_bundle Character. The product bundle to order.
#' @param tools List. The tools to apply to the order.
#'
#' @return A JSON string representing the order request body.
#'
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

#' Build Order Tools
#'
#' Constructs the tools list for an order request, including a clip tool and
#' an optional harmonization tool.
#'
#' @param bbox A list with bounding box values.
#' @param harmonized Logical. Whether to include harmonization (default: FALSE).
#'
#' @return A list representing the tools for the order.
#'
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
