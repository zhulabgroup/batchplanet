# Read and combine processed data products

Reads and combines data files processed with this package(e.g., time
series, cleaned remote sensing index, or phenological metrics). Supports
reading a subset of data from specified sites and groups. Three data
products are supported:

- "ts": raw time series data

- "clean": cleaned time series data, with optionally calculated Enhanced
  Vegetation Index (EVI)

- "doy": extracted phenological metrics

## Usage

``` r
read_data_product(dir, v_site = NULL, v_group = NULL, product_type = "clean")
```

## Arguments

- dir:

  Character. Base directory containing processed data product files
  (e.g., the parent directory of "ts/", "clean/", or "doy/").

- v_site:

  Character vector, optional. Site identifiers to include; if `NULL`,
  all sites are included.

- v_group:

  Character vector, optional. Group identifiers to include; if `NULL`,
  all groups are included.

- product_type:

  Character. Type of product to read (e.g., "ts", "clean", or "doy").
  Default is "clean".

## Value

A data frame combining all matching data product files, with columns for
site and group.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example: Read all cleaned time series for two sites and one group
df_clean <- read_data_product(
  dir = "alldata/PSdata/",
  v_site = c("HARV", "SJER"),
  v_group = "Quercus",
  product_type = "clean"
)

# Example: Read all phenological metrics for a site
df_doy <- read_data_product(
  dir = "alldata/PSdata/",
  v_site = "SJER",
  product_type = "doy"
)
} # }
```
