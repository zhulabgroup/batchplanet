# Clean a single PlanetScope time series

Cleans a single time series data frame by removing low-quality data and
optionally calculating NDVI and/or EVI. Low-quality data are defined as:

- The sun elevation angle is less than 0 degrees (i.e., night time
  images).

- The reflectance values for any band are less than 0.

- The pixel was cloudy, had snow, ice, shadow, haze, or cloud.

- The usable data mask had algorithmic confidence in classification less
  than 80% for the pixel.

## Usage

``` r
clean_planetscope_time_series(
  df_ts,
  calculate_index = c("ndvi", "evi"),
  filter_range = list(ndvi = c(-1, 1), evi = c(0, 1))
)
```

## Arguments

- df_ts:

  Data frame. Raw time series data for a single site/group.

- calculate_index:

  Character vector. Specifies which vegetation indices to calculate.
  Supported options are NDVI and EVI. Inputs are case-insensitive. If
  `NULL`, no indices are calculated. (default: `c("ndvi", "evi")`).

- filter_range:

  List. A named list specifying the valid range for indices. To disable
  filtering for an index, set it to `NULL` or omit it. (default:
  `list(ndvi = c(-1, 1), evi = c(0, 1))`).

## Value

Data frame of cleaned time series, with NDVI and/or EVI if requested.

## Examples

``` r
if (FALSE) { # \dontrun{
df_clean <- clean_planetscope_time_series(
  df_ts = df_ts_example,
  calculate_index = c("ndvi", "evi"),
  filter_range = list(ndvi = c(-1, 1), evi = c(0, 1))
)
} # }
```
