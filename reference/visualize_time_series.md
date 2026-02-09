# Visualize time series interactively

Generates an interactive plotly time series plot from a data frame,
optionally overlaying phenological events.

## Usage

``` r
visualize_time_series(
  df_ts,
  df_doy = NULL,
  var = "value",
  ylab = "Value",
  smooth = FALSE,
  lambda = 50,
  facet_var = NULL,
  color_palette = "viridis"
)
```

## Arguments

- df_ts:

  Data frame. Must contain either a `time` (POSIX) or `date` (Date)
  column, an `id` column, and the variable to plot.

- df_doy:

  Optional data frame. Time of phenological events with columns `year`
  and `doy`. Default: `NULL`.

- var:

  Character. Name of the column in `df_ts` to plot (default: `"value"`).

- ylab:

  Character. Label for the y-axis (default: `"Value"`).

- smooth:

  Logical. If `TRUE`, applies gap-filling and smoothing to data points
  with Whittaker smoothing (default: `FALSE`).

- lambda:

  Numeric. Smoothing parameter for Whittaker smoothing (default: 50).

- facet_var:

  Character or `NULL`. Column name in `df_ts` and `df_doy` to facet by
  (e.g., `"site"` or `"id"`).

- color_palette:

  Character. Name of a viridis palette for line colors (default:
  `"viridis"`).

## Value

An interactive `plotly` object when supported, a static `ggplot` object
otherwise.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example: Visualize a time series with phenological events
visualize_time_series(
  df_ts = df_ts,
  df_doy = df_doy,
  var = "value",
  ylab = "Value",
  smooth = TRUE,
  lambda = 50,
  facet_var = "id",
  color_palette = "viridis"
)
} # }
```
