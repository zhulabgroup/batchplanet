# test-visualize_true_color_imagery.R
#
# Unit tests for visualize_true_color_imagery_batch, visualize_true_color_imagery

library(testthat)
library(batchplanet)

test_that("visualize_true_color_imagery runs with NEON example raster", {
  tif_path <- system.file(
    "extdata/NEON/raw/HARV/HARV_2025_1_31/20250130_151713_45_24c3_3B_AnalyticMS_SR_harmonized_clip.tif",
    package = "batchplanet"
  )
  skip_if(tif_path == "", "Example NEON raster not found")
  plt <- visualize_true_color_imagery(file = tif_path)
  expect_true("gg" %in% class(plt) || "ggplot" %in% class(plt))
})

test_that("visualize_true_color_imagery_batch runs with NEON example directory", {
  dir_path <- system.file("extdata/NEON/raw/HARV", package = "batchplanet")
  skip_if(dir_path == "", "Example NEON directory not found")
  # Just check that the function can be called (do not launch the app in test)
  expect_error(
    visualize_true_color_imagery_batch(dir = dir_path, cloud_lim = 1),
    NA
  )
})
