# Visualize coordinates from a data frame

Creates a scatter plot of coordinate data.

## Usage

``` r
visualize_coordinates(df_coordinates)
```

## Arguments

- df_coordinates:

  Data frame containing longitude and latitude columns.

## Value

An interactive `leaflet` map object when supported, a static `ggplot`
object otherwise.

## Examples

``` r
if (FALSE) { # \dontrun{
visualize_coordinates(df_coordinates)
} # }
```
