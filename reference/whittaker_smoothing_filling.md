# Smooth and gap-fill a time series using Whittaker smoothing

Applies weighted Whittaker smoothing to a numeric time series, filling
short gaps (NAs) and smoothing the signal at the same time. This
function is useful for environmental time series with missing or noisy
data. Note that the input time series need to be sampled at a regular
interval. If not, you need to resample it first, with NAs inserted at
the missing time points.

## Usage

``` r
whittaker_smoothing_filling(x, lambda, maxgap = Inf, minseg = 2)
```

## Arguments

- x:

  Numeric vector. The time series signal to be smoothed (e.g., EVI,
  NDVI, or other index).

- lambda:

  Numeric. Smoothing parameter; larger values result in a smoother
  output.

- maxgap:

  Numeric. Maximum number of consecutive NAs to interpolate (default:
  Inf). Gaps longer than this will remain NA.

- minseg:

  Numeric. Minimum segment length for smoothing (default: 2). Segments
  shorter than this will be replaced with NA.

## Value

Numeric vector. The smoothed and gap-filled signal, with the same length
as `x`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Simulate a noisy, gappy time series
set.seed(42)
t <- 1:365
x <- sin(2 * pi * t / 365) + rnorm(365, sd = 0.1)
x[sample(1:365, 30)] <- NA

# Smooth and fill gaps
x_sm <- whittaker_smoothing_filling(x, lambda = 50, maxgap = 14, minseg = 2)
plot(t, x, type = "p", col = "grey", main = "Whittaker Smoothing")
lines(t, x_sm, col = "darkgreen", lwd = 2)
} # }
```
