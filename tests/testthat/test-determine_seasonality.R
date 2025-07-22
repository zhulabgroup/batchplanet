# test-determine_seasonality.R
#
# Unit tests for determine_seasonality():
# - Flat series should be non-seasonal.
# - Sinusoidal series should be seasonal.
#

library(testthat)
library(batchplanet)

test_that("Flat series is not seasonal", {
  ts_flat <- seq(0, 1, length.out = 100)
  result <- suppressWarnings(determine_seasonality(ts_flat))
  expect_false(result)
})

test_that("Sinusoidal series is seasonal", {
  ts_sin <- sin(seq(0, 2 * pi, length.out = 100))
  result <- suppressWarnings(determine_seasonality(ts_sin))
  expect_true(result)
})

test_that("determine_seasonality handles noise and missing data", {
  set.seed(123)
  t <- 1:100
  ts_seasonal <- sin(2 * pi * t / 100) + rnorm(100, sd = 0.1)
  ts_seasonal[sample(1:100, 10)] <- NA
  result <- suppressWarnings(determine_seasonality(ts_seasonal))
  expect_true(result)
})
