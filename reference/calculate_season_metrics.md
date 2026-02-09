# Calculate start/end of season metrics for a single time series

Processes a remote sensing index time series for a single location in
one year by gap-filling and smoothing (using Whittaker smoothing),
testing for seasonality, and calculating day-of-year (DOY) when the
index first goes above or goes below threshold(s) specified in
`df_thres`. The input time series should ideally be extended to include
the end of the previous year and the beginning of the next year to
capture early- and late-year events.

## Usage

``` r
calculate_season_metrics(
  df_index,
  df_thres,
  min_days,
  check_seasonality = T,
  var_index = "evi"
)
```

## Arguments

- df_index:

  Data frame of remote sensing index time series at one location in one
  year. Must contain columns `doy` and the index of interest.

- df_thres:

  Data frame containing candidate threshold values, with columns
  `direction` ("up"/"down") and `threshold` (numeric 0–1).

- min_days:

  Numeric. Minimum required number of non-NA data points in one year
  (default: 80).

- check_seasonality:

  Logical. If `TRUE`, tests for significant seasonal changes before
  calculating start/end of season metrics (default: `TRUE`).

- var_index:

  Character. Name of the index column in `df_index` to analyze (default:
  "evi").

## Value

A data frame of the timing (DOY) of threshold-crossing events, or `NULL`
if there are fewer than `min_days` valid data points or if the index
does not show a seasonal pattern.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example: Calculate start of season metrics for a single cleaned time series
df_thres <- set_thresholds(thres_up = c(0.3, 0.5), thres_down = NULL)
df_metrics <- calculate_season_metrics(
  df_index = df_clean,
  df_thres = df_thres,
  min_days = 80,
  check_seasonality = TRUE,
  var_index = "evi"
)
} # }
```
