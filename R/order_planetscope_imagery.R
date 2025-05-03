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
                                      mostrecent) {
  if (mostrecent > 0) {
    items <- sort(items, decreasing = TRUE)[1:mostrecent]
    message(paste("Selected", mostrecent, "most recent images."))
  }

  tools <- build_order_tools(bbox, harmonized)
  order_json <- build_order_request_body(order_name, items, item_name, product_bundle, tools)

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
