#' Visualize Time Series
#'
#' Creates a customizable time series plot with improved labeling and annotations.
#'
#' @param df_ts Data frame containing time series data.
#' @param v_site Character vector of site names (defaults to all sites).
#' @param v_group Character vector of group names (defaults to all groups).
#' @param v_id Character vector of point identifiers (defaults to a random sample).
#' @param v_year Numeric vector of years to include (defaults to all years).
#' @param n_id Numeric. Number of IDs to sample if `v_id` is NULL (default: 3).
#' @param var Character. Column name for the variable to plot (default: "value").
#' @param ylab Character. Label for the Y-axis.
#' @param smooth Logical. Whether to apply loess smoothing (default: FALSE).
#' @param annotate Logical. Whether to add key event markers (default: TRUE).
#' @param color_palette Character. A color-blind friendly palette option (default: "viridis").
#' @param facet Optional. Column name for faceting.
#' @param save_plot Logical. Whether to save the plot (default: FALSE).
#' @param plot_path Character. Directory to save the plot (default: "plots").
#'
#' @return A ggplot object.
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

  p <- ggplot(df_coordinates, aes(
    x = lon, y = lat,
    text = str_c("ID: ", id, "<br>Longitude: ", lon, "<br>Latitude: ", lat)
  )) +
    geom_point(size = 0.5) +
    labs(x = "Longitude", y = "Latitude")

  p <- apply_plot_style(p)

  plotly::ggplotly(p, tooltip = "text")
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
