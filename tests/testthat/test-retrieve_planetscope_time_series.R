# test-retrieve_planetscope_time_series.R
#
# Unit tests for retrieve_planetscope_time_series_batch, retrieve_planetscope_time_series, retrieve_planetscope_time_series_sitegroup, retrieve_raster_data

library(testthat)
library(batchplanet)

test_that("retrieve_planetscope_time_series_batch runs (skipped by default)", {
  skip_on_cran()
  skip("API/long-running test not run by default")
  # Example (do not actually run):
  # retrieve_planetscope_time_series_batch(dir = tempdir(), df_coordinates = data.frame())
  expect_true(TRUE)
})

test_that("retrieve_planetscope_time_series runs (skipped by default)", {
  skip_on_cran()
  skip("API/long-running test not run by default")
  # Example (do not actually run):
  # retrieve_planetscope_time_series(dir_site = tempdir(), sf_coordinates = sf::st_sf())
  expect_true(TRUE)
})

test_that("retrieve_planetscope_time_series_sitegroup runs (skipped by default)", {
  skip_on_cran()
  skip("API/long-running test not run by default")
  # Example (do not actually run):
  # retrieve_planetscope_time_series_sitegroup(dir_site = tempdir(), sf_coordinates = sf::st_sf())
  expect_true(TRUE)
})

test_that("retrieve_raster_data runs (skipped by default)", {
  skip_on_cran()
  skip("API/long-running test not run by default")
  # Example (do not actually run):
  # retrieve_raster_data(dir_site = tempdir(), sf_coordinates = sf::st_sf(), type = "sr", num_cores = 1)
  expect_true(TRUE)
})
