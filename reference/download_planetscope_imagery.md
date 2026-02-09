# Download a PlanetScope order

Downloads all files for a given PlanetScope order ID, saving them to the
specified folder. The function will wait until the order is successfully
processed by Planet API, partially processed, fail to be processed, or
is cancelled.

## Usage

``` r
download_planetscope_imagery(
  order_id,
  exportfolder,
  api_key,
  overwrite = FALSE
)
```

## Arguments

- order_id:

  Character. The PlanetScope order ID.

- exportfolder:

  Character. Directory in which to save downloaded files (created if
  needed).

- api_key:

  Character. Your Planet API key.

- overwrite:

  Logical. If `TRUE`, existing files will be overwritten (default:
  `FALSE`).

## Value

Invisibly returns `NULL` and writes all imagery files to the specified
`exportfolder`.

## Examples

``` r
if (FALSE) { # \dontrun{
download_planetscope_imagery(
  order_id = "abc123-order-id",
  exportfolder = "data/raw/SJER/SJER_202405_121_151",
  api_key = set_api_key(),
  overwrite = TRUE
)
} # }
```
