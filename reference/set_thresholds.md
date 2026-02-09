# Specify thresholds for detecting start/end of season

Creates a data frame with threshold values for both increasing ("up")
and decreasing ("down") trends. These values are used to detect
threshold-crossing events in the time series that indicate the start/end
of season.

## Usage

``` r
set_thresholds(
  thres_up = round(seq(from = 0, to = 1, by = 0.1), 1),
  thres_down = round(seq(from = 1, to = 0, by = -0.1), 1)
)
```

## Arguments

- thres_up:

  Numeric vector. Threshold values for increasing trends (default:
  seq(from = 0, to = 1, by = 0.1) \|\> round(1)).

- thres_down:

  Numeric vector. Threshold values for decreasing trends (default:
  seq(from = 1, to = 0, by = -0.1) \|\> round(1)).

## Value

A data frame with two columns:

- direction:

  A character vector indicating "up" for increasing trends or "down" for
  decreasing trends.

- threshold:

  A numeric vector containing threshold values between 0 and 1.

## Examples

``` r
# Get default thresholds (for increasing and decreasing trends)
default_thres <- set_thresholds()

# Get thresholds with custom rule
custom_thres <- set_thresholds(thres_up = c(0.3, 0.5), thres_down = NULL)
```
