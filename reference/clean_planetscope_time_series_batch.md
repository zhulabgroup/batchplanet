# Clean PlanetScope time series for multiple sites and groups

Cleans raw time series data for all sites and groups, removing
low-quality data and optionally calculating NDVI and/or EVI. Low-quality
data are defined as:

- The sun elevation angle is less than 0 degrees (i.e., night time
  images).

- The reflectance values for any band are less than 0.

- The pixel was cloudy, had snow, ice, shadow, haze, or cloud.

- The usable data mask had algorithmic confidence in classification less
  than 80% for the pixel. Results are saved as `.rds` files under the
  `clean/` subdirectory within `dir`, prefixed `clean_`.

## Usage

``` r
clean_planetscope_time_series_batch(
  dir,
  v_site = NULL,
  v_group = NULL,
  num_cores = 3,
  calculate_index = c("ndvi", "evi"),
  filter_range = list(ndvi = c(-1, 1), evi = c(0, 1))
)
```

## Arguments

- dir:

  Character. Base directory containing raw time series files (expects a
  `ts/` subdirectory).

- v_site:

  Character vector, optional. Site identifiers to process; if `NULL`,
  all sites in `ts/` are included.

- v_group:

  Character vector, optional. Group identifiers to process; if `NULL`,
  all groups in filenames are included.

- num_cores:

  Integer. Number of parallel workers to use (default: 3).

- calculate_index:

  Character vector. Specifies which vegetation indices to calculate.
  Supported options are NDVI and EVI. Inputs are case-insensitive. If
  `NULL`, no indices are calculated. (default: `c("ndvi", "evi")`).

- filter_range:

  List. A named list specifying the valid range for indices. To disable
  filtering for an index, set it to `NULL` or omit it. (default:
  `list(ndvi = c(-1, 1), evi = c(0, 1))`).

## Value

Invisibly returns `NULL` and saves cleaned time series as `.rds` files
in the `clean/` subdirectory of `dir`.

## Examples

``` r
if (FALSE) { # \dontrun{
clean_planetscope_time_series_batch(
  dir = "alldata/PSdata/",
  v_site = c("HARV", "SJER"),
  v_group = c("Acer", "Quercus"),
  num_cores = 3,
  calculate_index = c("ndvi", "evi"),
  filter_range = list(ndvi = c(-1, 1), evi = c(0, 1))
)
} # }
```
