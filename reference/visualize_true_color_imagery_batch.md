# Launch a Shiny App to Visualize True-Color Imagery

Starts an interactive Shiny app for browsing and visualizing true-color
PlanetScope imagery in a directory.

## Usage

``` r
visualize_true_color_imagery_batch(dir, df_coordinates = NULL, cloud_lim = 1)
```

## Arguments

- dir:

  Character. Base directory containing raw imagery (expects a `raw/`
  subdirectory).

- df_coordinates:

  Optional data frame. Point locations to overlay; must contain `lon`,
  `lat`, and `site`. Default: `NULL`.

- cloud_lim:

  Numeric. Maximum allowed cloud cover (0-1, default: 1). Only images
  with cloud cover \<= cloud_lim are shown.

## Value

None. Launches a Shiny app in the default web browser.

## Examples

``` r
if (FALSE) { # \dontrun{
visualize_true_color_imagery_batch(
  dir = "alldata/PSdata/",
  df_coordinates = df_coordinates,
  cloud_lim = 0.1
)
} # }
```
