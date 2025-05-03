#' Apply Default Plot Style
#'
#' Adds the default style layers to a ggplot object.
#'
#' @param p A ggplot object.
#' @param color_palette A character string specifying the color palette.
#' @param include_scales Logical. Whether to include color/fill scales.
#'
#' @return A ggplot object with the style layers applied.
util_plot_style <- function(p) {
  p <- p +
    theme_minimal() +
    theme(text = element_text(size = 12)) +
    theme(legend.position = "none") +
    scale_color_viridis_d(option = "viridis") +
    scale_fill_viridis_d(option = "viridis")

  return(p)
}
