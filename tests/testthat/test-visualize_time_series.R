# test-visualize_time_series.R
#
# Unit tests for visualize_time_series, visualize_coordinates

library(testthat)
library(batchplanet)

test_that("visualize_time_series runs (skipped by default)", {
  skip_on_cran()
  skip("Plotting/interactive test not run by default")
  # Example (do not actually run):
  # visualize_time_series(df_ts = data.frame(time = Sys.time(), id = 1, value = 1))
  expect_true(TRUE)
})

test_that("visualize_coordinates runs (skipped by default)", {
  skip_on_cran()
  skip("Plotting/interactive test not run by default")
  # Example (do not actually run):
  # visualize_coordinates(df_coordinates = data.frame(lon = 0, lat = 0))
  expect_true(TRUE)
})
