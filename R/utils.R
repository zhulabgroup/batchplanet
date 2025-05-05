#' Apply Default Plot Style
#'
#' Adds the default style layers to a ggplot object.
#'
#' @param p A ggplot object.
#' @param color_palette A character string specifying the color palette.
#' @param include_scales Logical. Whether to include color/fill scales.
#'
#' @return A ggplot object with the style layers applied.
apply_plot_style <- function(p) {
  p <- p +
    theme_minimal() +
    theme(text = element_text(size = 12)) +
    theme(legend.position = "none") +
    scale_color_viridis_d(option = "viridis") +
    scale_fill_viridis_d(option = "viridis")

  return(p)
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
set_bbox <- function(df_coords, location, buffer = 0.0005) {
  # Filter the coordinate data for the specified location and remove any rows with missing values.
  df_location <- df_coords %>%
    filter(site == {{ location }}) %>%
    drop_na(lon, lat)

  # Check if any valid coordinates are found
  if (nrow(df_location) == 0) {
    warning("No valid coordinates found for location: ", location)
    return(NULL)
  }

  # Compute the minimum and maximum longitude and latitude, and apply the buffer.
  bbox <- sf::st_bbox(c(
    xmin = min(df_location$lon) - buffer,
    xmax = max(df_location$lon) + buffer,
    ymin = min(df_location$lat) - buffer,
    ymax = max(df_location$lat) + buffer
  ))

  return(bbox)
}
