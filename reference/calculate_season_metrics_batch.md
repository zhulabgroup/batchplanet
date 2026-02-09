# Calculate start/end of season metrics for time series from multiple sites and groups

Reads remote sensing index time series files from the `clean/`
subdirectory within the specified `dir`. For each site, group, and year
combination, the function extends the time series to include the end of
the previous year and the beginning of the next year (to capture early-
and late-year events), then calls
[`calculate_season_metrics()`](https://zhulabgroup.github.io/BatchPlanet/reference/calculate_season_metrics.md)
to calculate start/end of season metrics for each time series, which are
measured by the day-of-year (DOY) when the index first goes above or
goes below specified threshold(s). Results are saved as `.rds` files
under the `doy/` subdirectory within `dir`, prefixed `doy_`.

## Usage

``` r
calculate_season_metrics_batch(
  dir,
  v_site = NULL,
  v_group = NULL,
  v_year = NULL,
  df_thres = NULL,
  var_index = "evi",
  min_days = 80,
  check_seasonality = T,
  extend_to_previous_year = 275,
  extend_to_next_year = 90,
  num_cores = 3
)
```

## Arguments

- dir:

  Character. Base directory containing remote sensing index files
  (expects `clean/` subdirectory).

- v_site:

  Character vector, optional. Site identifiers to process; if `NULL`,
  all sites are included.

- v_group:

  Character vector, optional. Group identifiers to process; if `NULL`,
  all groups are included.

- v_year:

  Integer vector, optional. Years to process; if `NULL`, all years in
  the data are included.

- df_thres:

  Data frame of thresholds as from
  [`set_thresholds()`](https://zhulabgroup.github.io/BatchPlanet/reference/set_thresholds.md);
  if `NULL`, uses default thresholds.

- var_index:

  Character. Name of the remote sensing index column in the input data
  frame `df_index` to analyze (default: "evi").

- min_days:

  Numeric. Minimum required number of non-NA data points in one year
  (default: 80).

- check_seasonality:

  Logical. If `TRUE`, tests for significant seasonal changes before
  calculating start/end of season metrics (default: `TRUE`).

- extend_to_previous_year:

  Integer. Day of year to extend backward to, to capture early-year
  events (default: 275).

- extend_to_next_year:

  Integer. Day of year to extend forward to, to capture late-year events
  (default: 90).

- num_cores:

  Integer. Number of parallel workers for processing (default: 3).

## Value

Invisibly returns `NULL` and saves calculated start/end of season
metrics as `.rds` files in the `doy/` subdirectory of `dir`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example: Calculate start of season metrics for two sites and two groups
df_thres <- set_thresholds(thres_up = c(0.3, 0.5))
calculate_season_metrics_batch(
  dir = "alldata/PSdata/",
  v_site = c("Site1", "Site2"),
  v_group = c("Group1", "Group2"),
  v_year = c(2023, 2024),
  df_thres = df_thres,
  var_index = "evi",
  min_days = 80,
  check_seasonality = TRUE,
  extend_to_previous_year = 275,
  extend_to_next_year = 90,
  num_cores = 3
)
} # }
```
