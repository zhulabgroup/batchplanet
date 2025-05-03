#' Visualize Coordinates from a Dataframe
#'
#' Creates a scatter plot of coordinate data.
#'
#' @param df_points Data frame containing longitude and latitude columns.
#' @param save_plot Logical. If TRUE, saves the plot to disk.
#' @param plot_path Character. Directory where the plot will be saved (default: "plots").
#' @param color_palette Character. The palette option for styling (default: "colorblind").
#'
#' @return A ggplot object displaying the coordinate points.
#' @export
visualize_coordinates <- function(df_coordinates) {
  # Validate required columns
  if (!all(c("lon", "lat") %in% names(df_coordinates))) {
    stop("Data frame must contain 'lon' and 'lat' columns.")
  }

  p <- ggplot(df_coordinates, aes(x = lon, y = lat)) +
    geom_point(size = 0.5) +
    labs(x = "Longitude", y = "Latitude")

  p <- util_plot_style(p)

  return(p)
}
