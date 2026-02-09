# Download PlanetScope imagery for multiple sites and years

Downloads all ordered PlanetScope imagery for specified sites and years.
The function will wait until the order is successfully processed by
Planet API, and then download the files.

## Usage

``` r
download_planetscope_imagery_batch(
  dir,
  v_site = NULL,
  v_year = 2017:(as.integer(format(Sys.Date(), "%Y"))),
  v_month = 1:12,
  setting,
  num_cores = 12,
  overwrite = F
)
```

## Arguments

- dir:

  Character. Base directory where the raw satellite data will be stored,
  expects a `raw/` subdirectory.

- v_site:

  Character vector, optional. Site names to filter; if `NULL`, all sites
  are used.

- v_year:

  Numeric vector. Years for which to download data. Default is from 2017
  to the current year.

- v_month:

  Integer vector. Months (1-12) to download (default: 1:12, i.e., all
  months).

- setting:

  List. Contains API settings including the API key (i.e.,
  `setting$api_key`).

- num_cores:

  Integer. Number of parallel workers to use (default: 12).

- overwrite:

  Logical. If `TRUE`, existing files will be overwritten (default:
  `FALSE`).

## Value

Invisibly returns `NULL` and saves downloaded imagery to the `raw/`
subdirectory of the specified `dir`.

## Examples

``` r
if (FALSE) { # \dontrun{
download_planetscope_imagery_batch(
  dir = "alldata/PSdata/",
  v_site = c("HARV", "SJER"),
  v_year = 2025,
  v_month = 4:6,
  setting = setting,
  num_cores = 3,
  overwrite = FALSE
)
} # }
```
