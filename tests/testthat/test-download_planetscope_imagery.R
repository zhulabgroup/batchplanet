# test-download_planetscope_imagery.R
#
# Unit tests for download_planetscope_imagery_batch, download_planetscope_imagery_siteyear, download_planetscope_imagery

library(testthat)
library(batchplanet)

test_that("download_planetscope_imagery_batch runs (skipped by default)", {
  skip_on_cran()
  skip("Test that requires API not run by default")
  # Example (do not actually run):
  # setting <- set_planetscope_parameters(api_key = "dummy", item_name = "PSScene", asset = "ortho_analytic_4b_sr", product_bundle = "analytic_sr_udm2", cloud_lim = 1, harmonized = TRUE)
  # download_planetscope_imagery_batch(dir = tempdir(), setting = setting)
  expect_true(TRUE)
})

test_that("download_planetscope_imagery runs (skipped by default)", {
  skip_on_cran()
  skip("Test that requires API not run by default")
  # Example (do not actually run):
  # download_planetscope_imagery(order_id = "dummy", exportfolder = tempdir(), api_key = set_api_key())
  expect_true(TRUE)
})
