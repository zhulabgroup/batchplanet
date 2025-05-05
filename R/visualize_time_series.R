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
      mutate(value_smooth = util_fill_whit(value, lambda = lambda)) %>%
      ungroup()
  }

  p <- ggplot() +
    geom_point(data = df_ts, aes(x = x_var, y = value, color = id_shuffle, group = id_shuffle)) +
    labs(x = "Date", y = ylab, color = "ID")

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
    p <- p +
      geom_vline(data = df_doy, aes(xintercept = date_doy), col = "dark green") +
      geom_rect(data = df_doy, aes(xmin = date_start, xmax = date_end, ymin = -Inf, ymax = Inf), alpha = 0.1, fill = "dark green")
  }

  if (!is.null(facet_var)) {
    p <- p + facet_wrap(as.formula(paste("~", facet_var)), ncol = 1)
  }

  p <- apply_plot_style(p)

  return(p)
}
