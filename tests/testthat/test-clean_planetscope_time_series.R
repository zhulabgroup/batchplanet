# test-clean_planetscope_time_series.R
#
# Unit tests for clean_planetscope_time_series_batch and clean_planetscope_time_series

library(testthat)
library(batchplanet)

test_that("clean_planetscope_time_series_batch runs (skipped by default)", {
  skip_on_cran()
  skip("Long-running or file-based test not run by default")
  # Example (do not actually run):
  # clean_planetscope_time_series_batch(dir = tempdir())
  expect_true(TRUE)
})

test_that("clean_planetscope_time_series cleans dummy data", {
  # Minimal test with dummy data
  df_ts <- data.frame(
    id = 1,
    lon = 0,
    lat = 0,
    time = as.POSIXct("2020-01-01 12:00:00"),
    sun_elevation = 45,
    red = 100,
    green = 100,
    blue = 100,
    nir = 200,
    clear = 1,
    snow = 0,
    shadow = 0,
    haze_light = 0,
    haze_heavy = 0,
    cloud = 0,
    confidence = 100
  )
  df_clean <- clean_planetscope_time_series(df_ts, calculate_evi = TRUE)
  expect_s3_class(df_clean, "data.frame")
  expect_true("evi" %in% names(df_clean))
})
