# test-visualize_time_series.R
#
# Unit tests for visualize_time_series, visualize_coordinates

library(testthat)
library(batchplanet)

test_that("visualize_time_series runs with NEON example data", {
  # Use a small subset of the NEON clean time series
  ts_path <- system.file("extdata/NEON/clean/clean_HARV_Quercus.rds", package = "batchplanet")
  skip_if(ts_path == "", "Example NEON clean time series not found")
  df_ts <- readRDS(ts_path)
  df_ts <- head(df_ts, 20) # Use a small subset for speed
  plt <- visualize_time_series(df_ts = df_ts, var = "evi")
  expect_true("plotly" %in% class(plt))
})

test_that("visualize_coordinates runs with NEON example data", {
  coords_path <- system.file("extdata/NEON/example_neon_coordinates.csv", package = "batchplanet")
  skip_if(coords_path == "", "Example NEON coordinates not found")
  df_coordinates <- read.csv(coords_path)
  df_coordinates <- head(df_coordinates, 10) # Use a small subset for speed
  plt <- visualize_coordinates(df_coordinates)
  expect_true("gg" %in% class(plt) || "ggplot" %in% class(plt))
})
