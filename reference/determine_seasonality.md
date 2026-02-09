# Test for seasonality in a time series Using AIC-based model selection

Fits a simple linear regression model and segmented regression models
(with 1 to 3 breakpoints) to a numeric time series. Compares their
Akaike Information Criterion (AIC) values, with a penalty parameter `k`,
to determine if the time series is best described as flat (no
seasonality) or as having significant seasonal changes

## Usage

``` r
determine_seasonality(ts, doy = 1:length(ts), k = 50)
```

## Arguments

- ts:

  Numeric vector. The time series signal to evaluate (e.g., EVI, NDVI,
  or other index).

- doy:

  Numeric vector. The day-of-year indices corresponding to the time
  series. Default is `1:length(ts)`. If your `ts` is not daily, you need
  to provide the day-of-year indices.

- k:

  Numeric. Penalty parameter for the AIC calculation. Higher values make
  the function more conservative in detecting seasonality. Default is
  50.

## Value

Logical. Returns `TRUE` if the time series is seasonal (segmented model
preferred), or `FALSE` if the time series is flat (linear model
preferred).

## Examples

``` r
if (FALSE) { # \dontrun{
# Simulate a seasonal time series with noise and missing values
t <- 1:365
simulate_ts <- sin(2 * pi * t / 365) + rnorm(365, sd = 0.1)
simulate_ts[sample(1:365, 30)] <- NA # introduce some missing data

# Test for seasonality
determine_seasonality(ts = simulate_ts, k = 50)

# Example with a flat (non-seasonal) time series
flat_ts <- rnorm(365, mean = 0.5, sd = 0.05)
determine_seasonality(ts = flat_ts)
} # }
```
