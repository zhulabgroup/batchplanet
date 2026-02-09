# Retrieve PlanetScope time series for multiple sites and groups

Extracts time series data from PlanetScope imagery for all points in a
coordinate data frame, grouped by site and group. For each site and
group, reflectance, QA, and metadata are extracted and combined Results
are saved as `.rds` files under the `ts/` subdirectory of the specified
`dir`, prefixed `ts_`.

## Usage

``` r
retrieve_planetscope_time_series_batch(
  dir,
  df_coordinates,
  v_site = NULL,
  v_group = NULL,
  max_sample = 2000,
  num_cores = 12
)
```

## Arguments

- dir:

  Character. Base directory containing raw PlanetScope data (expects a
  `raw/` subdirectory).

- df_coordinates:

  Data frame with columns `site`, `lon`, `lat`, `id`, and optionally
  `group`.

- v_site:

  Character vector, optional. Site identifiers to process; if `NULL`,
  all sites in `df_coordinates` are included.

- v_group:

  Character vector, optional. Group identifiers to process; if `NULL`,
  all groups in `df_coordinates` are included.

- max_sample:

  Integer. Maximum number of samples per group (default: 2000). This is
  to avoid processing too many points at once, which can be slow.

- num_cores:

  Integer. Number of parallel workers for processing (default: 12).

## Value

Invisibly returns `NULL` and saves one `.rds` file per site-group to the
`ts/` subdirectory in the specified `dir`.

## Examples

``` r
if (FALSE) { # \dontrun{
retrieve_planetscope_time_series_batch(
  dir = "alldata/PSdata/",
  df_coordinates = df_coordinates,
  v_site = c("HARV", "SJER"),
  v_group = c("Acer", "Quercus"),
  max_sample = 2000,
  num_cores = 12
)
} # }
```
