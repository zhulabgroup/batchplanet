# Visualize true-color raster imagery

Reads a multi-band raster, converts the red, green, and blue bands to
normalized RGB values, and creates a ggplot2 tile plot of the true-color
composite. Optionally overlays point locations. Automatically normalizes
image brightness to the global average (computed at app startup) for
legibility, then applies the user brightness slider (0-10, default 5) as
a multiplier.

## Usage

``` r
visualize_true_color_imagery(
  file,
  df_coordinates = NULL,
  brightness = 5,
  global_brightness = 0.05
)
```

## Arguments

- file:

  Character. Path to a multi-band raster file (e.g., PlanetScope SR
  clip).

- df_coordinates:

  Optional data frame. Point locations to overlay; must contain `lon`,
  `lat`, and `site`. Default: `NULL`.

- brightness:

  Numeric. Brightness multiplier for RGB values (slider value, default:
  5).

- global_brightness:

  Numeric. The global average brightness (from sampled images at app
  startup).

## Value

A ggplot2 object.

## Examples

``` r
if (FALSE) { # \dontrun{
visualize_true_color_imagery(
  file = "alldata/PSdata/raw/SJER/20240501_SR_harmonized_clip.tif",
  df_coordinates = df_coordinates_SJER,
  brightness = 5,
  global_brightness = 0.05
)
} # }
```
