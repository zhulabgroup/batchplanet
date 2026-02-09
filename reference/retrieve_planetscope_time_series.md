# Retrieve a single set of PlanetScope time series

Extracts and combines reflectance, QA, and metadata for a set of spatial
points.

## Usage

``` r
retrieve_planetscope_time_series(dir_site, sf_coordinates, num_cores = 12)
```

## Arguments

- dir_site:

  Character. Path to the site-specific raw data directory.

- sf_coordinates:

  An `sf` object with point coordinates (must have an `id` column).

- num_cores:

  Integer. Number of parallel workers for processing (default: 12).

## Value

Data frame with columns for point ID, coordinates, reflectance bands,
QA, and metadata.

## Examples

``` r
if (FALSE) { # \dontrun{
df_coordinates_example <- df_coordinates |> dplyr::filter(site == "SJER", group == "Quercus")
df_ts_example <- retrieve_planetscope_time_series(
  dir_site = file.path("alldata/PSdata/raw", "SJER"),
  sf_coordinates = sf::st_as_sf(df_coordinates_example, coords = c("lon", "lat"), crs = 4326),
  num_cores = 12
)
} # }
```
