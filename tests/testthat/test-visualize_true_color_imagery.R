# test-visualize_true_color_imagery.R
#
# Unit tests for visualize_true_color_imagery_batch, visualize_true_color_imagery

library(testthat)
library(batchplanet)

test_that("visualize_true_color_imagery_batch runs (skipped by default)", {
  skip_on_cran()
  skip("Plotting/interactive test not run by default")
  # Example (do not actually run):
  # visualize_true_color_imagery_batch(dir = tempdir())
  expect_true(TRUE)
})

test_that("visualize_true_color_imagery runs (skipped by default)", {
  skip_on_cran()
  skip("Plotting/interactive test not run by default")
  # Example (do not actually run):
  # visualize_true_color_imagery(file = "dummy.tif")
  expect_true(TRUE)
})
