# test-visualize_true_color_imagery.R
#
# Unit tests for visualize_true_color_imagery_batch, visualize_true_color_imagery, visualize_coordinates

library(testthat)
library(batchplanet)

test_that("visualize_true_color_imagery_batch runs with NEON example directory", {
  dir_path <- system.file("extdata/NEON/", package = "batchplanet")
  # Just check that the function can be called (do not launch the app in test)
  expect_error(
    visualize_true_color_imagery_batch(dir = dir_path, cloud_lim = 0.1),
    NA
  )
})

test_that("visualize_true_color_imagery runs with NEON example raster", {
  tif_path <- system.file(
    "extdata/NEON/raw/SJER/SJER_2025_60_90/20250325_183148_95_24c5_3B_AnalyticMS_SR_harmonized_clip.tif",
    package = "batchplanet"
  )
  plt <- visualize_true_color_imagery(file = tif_path)
  expect_true("gg" %in% class(plt) || "ggplot" %in% class(plt))
})

test_that("visualize_coordinates runs with NEON example data", {
  df_coordinates <- system.file("extdata/NEON/example_neon_coordinates.csv", package = "batchplanet") %>% read_csv()
  plt <- visualize_coordinates(df_coordinates)
  expect_true("plotly" %in% class(plt))
})
