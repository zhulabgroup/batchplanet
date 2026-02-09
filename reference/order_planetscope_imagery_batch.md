# Order PlanetScope imagery for multiple sites and years

Orders PlanetScope imagery for all sites and years specified in a
coordinate data frame. Saves order IDs and metadata for each site and
year.

## Usage

``` r
order_planetscope_imagery_batch(
  dir,
  df_coordinates,
  v_site = NULL,
  v_year = 2017:(as.integer(format(Sys.Date(), "%Y"))),
  v_month = 1:12,
  setting
)
```

## Arguments

- dir:

  Character. Base directory for saving orders, expects a `raw/`
  subdirectory.

- df_coordinates:

  Data frame of coordinates of interest with columns `site`, `lon`,
  `lat`, and `id`.

- v_site:

  Character vector, optional. Site names to process; if `NULL`, all
  unique sites in `df_coordinates` are used.

- v_year:

  Numeric vector. Years to process (default: 2017 to current year).

- v_month:

  Integer vector. Months (1-12) to process (default: 1:12, i.e., all
  months).

- setting:

  List. API settings including API key, item name, asset type, cloud
  limit, product bundle, and harmonized flag.

## Value

Invisibly returns `NULL` and writes order metadata as `.rds` files under
`orders/` subdirectory of specified `dir`.

## Examples

``` r
if (FALSE) { # \dontrun{
order_planetscope_imagery_batch(
  dir = "alldata/PSdata/",
  df_coordinates = df_coordinates,
  v_site = c("HARV", "SJER"),
  v_year = 2025,
  v_month = 4:6,
  setting = setting
)
} # }
```
