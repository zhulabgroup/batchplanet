#' Visualize time series interactively
#'
#' Generates an interactive plotly time series plot from a data frame, optionally overlaying phenological events.
#'
#' @param df_ts Data frame. Must contain either a `time` (POSIX) or `date` (Date) column, an `id` column, and the variable to plot.
#' @param df_doy Optional data frame. Time of phenological events with columns `year` and `doy`. Default: `NULL`.
#' @param var Character. Name of the column in `df_ts` to plot (default: `"value"`).
#' @param ylab Character. Label for the y-axis (default: `"Value"`).
#' @param smooth Logical. If `TRUE`, applies gap-filling and smoothing to data points with Whittaker smoothing (default: `FALSE`).
#' @param lambda Numeric. Smoothing parameter for Whittaker smoothing (default: 50).
#' @param facet_var Character or `NULL`. Column name in `df_ts` and `df_doy` to facet by (e.g., `"site"` or `"id"`).
#' @param color_palette Character. Name of a viridis palette for line colors (default: `"viridis"`).
#'
#' @return An interactive plotly object.
#'
#' @examples
#' \dontrun{
#' # Example: Visualize a time series with phenological events
#' visualize_time_series(
#'   df_ts = df_ts,
#'   df_doy = df_doy,
#'   var = "value",
#'   ylab = "Value",
#'   smooth = TRUE,
#'   lambda = 50,
#'   facet_var = "id",
#'   color_palette = "viridis"
#' )
#' }
#'
#' @export
visualize_time_series <- function(df_ts,
                                  df_doy = NULL,
                                  var = "value",
                                  ylab = "Value",
                                  smooth = FALSE,
                                  lambda = 50,
                                  facet_var = NULL,
                                  color_palette = "viridis") {
  if ("time" %in% colnames(df_ts)) {
    df_ts <- df_ts %>% mutate(x_var = time)
  } else if ("date" %in% colnames(df_ts)) {
    df_ts <- df_ts %>% mutate(x_var = date)
  } else {
    stop("Data frame must contain a 'time' or 'date' column for time series visualization.")
  }

  set.seed(1)
  df_ts <- df_ts %>%
    mutate(value = !!sym(var)) %>%
    mutate(id_shuffle = factor(id, levels = sample(unique(id))))

  if (smooth) {
    df_ts <- df_ts %>%
      group_by(id) %>%
      mutate(value_smooth = whittaker_smoothing_filling(value, lambda = lambda)) %>%
      ungroup()
  }

  p <- ggplot(df_ts, aes(
    x = x_var, y = value, color = id_shuffle, group = id_shuffle,
    text = str_c("ID: ", id_shuffle, "<br>Time: ", x_var, "<br>Value: ", value)
  )) +
    geom_point() +
    labs(x = "Time", y = ylab, color = "ID")

  if (smooth) {
    p <- p +
      geom_line(data = df_ts, aes(x = x_var, y = value_smooth, color = id_shuffle, group = id_shuffle))
  }

  if (!is.null(df_doy)) {
    df_doy <- df_doy %>%
      mutate(
        date_doy = as.Date(str_c(year, "-01-01")) + doy - 1,
        date_start = as.Date(str_c(year, "-01-01")) + start - 1,
        date_end = as.Date(str_c(year, "-01-01")) + end - 1
      )
    y_min <- min(df_ts$value, na.rm = TRUE)
    y_max <- max(df_ts$value, na.rm = TRUE)

    p <- p +
      geom_segment(
        data = df_doy,
        aes(x = date_doy, xend = date_doy, y = y_min, yend = y_max),
        col = "dark green", ,
        inherit.aes = F
      ) +
      geom_rect(
        data = df_doy,
        aes(xmin = date_start, xmax = date_end, ymin = y_min, ymax = y_max),
        alpha = 0.1,
        fill = "dark green",
        inherit.aes = F
      )
  }

  if (!is.null(facet_var)) {
    p <- p + facet_wrap(as.formula(paste("~", facet_var)), ncol = 1)
  }

  p <- apply_plot_style(p)

  g <- plotly::ggplotly(p, tooltip = "text")
  # Disable hoverinfo for the DOY layers (background layers)
  for (i in 2:length(g$x$data)) {
    g$x$data[[i]]$hoverinfo <- "skip" # Skip hover for all layers except the first one
  }

  g
}

apply_plot_style <- function(p) {
  p <- p +
    theme_minimal() +
    theme(text = element_text(size = 12)) +
    theme(legend.position = "none") +
    scale_color_viridis_d(option = "viridis") +
    scale_fill_viridis_d(option = "viridis")

  return(p)
}
