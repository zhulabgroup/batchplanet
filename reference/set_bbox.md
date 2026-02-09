# Generate a Bounding Box from Coordinate Data

Creates a bounding box for a specified location based on a data frame of
coordinates. The function filters the data for the given location,
removes any rows with missing longitude or latitude values, and computes
the minimum and maximum coordinate values. A buffer is applied to expand
the bounding box by a specified distance (in degrees).

## Usage

``` r
set_bbox(df_coordinates, siteoi, buffer = 5e-04)
```

## Arguments

- df_coordinates:

  A data frame containing coordinate data. Must include the columns
  `site`, `lon`, and `lat`.

- siteoi:

  Character. The site identifier (i.e., the value in the `site` column)
  for which to generate the bounding box.

- buffer:

  Numeric. A buffer distance (in degrees) to expand the bounding box in
  all four directions. Default is 0.0005.

## Value

An object of class `bbox` (from the **sf** package) representing the
bounding box for the specified location, or `NULL` if no valid
coordinates are found.

## Examples

``` r
if (FALSE) { # \dontrun{
df_coords <- data.frame(
  site = c("SiteA", "SiteA", "SiteB"),
  lon = c(-77.05, -77.00, -76.95),
  lat = c(38.80, 38.85, 38.90)
)
bbox <- set_bbox(df_coords, "SiteA", buffer = 0.001)
} # }
```
