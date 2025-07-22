# test-download_planetscope_imagery.R
#
# Unit tests for download_planetscope_imagery_batch, download_planetscope_imagery_siteyear, download_planetscope_imagery

library(testthat)
library(batchplanet)

test_that("download_planetscope_imagery_batch runs (skipped by default)", {
  skip_on_cran()
  skip("API/long-running test not run by default")
  # Example (do not actually run):
  # download_planetscope_imagery_batch(dir = tempdir())
  expect_true(TRUE)
})

test_that("download_planetscope_imagery_siteyear runs (skipped by default)", {
  skip_on_cran()
  skip("API/long-running test not run by default")
  # Example (do not actually run):
  # download_planetscope_imagery_siteyear(dir_site = tempdir(), siteoi = "A", yearoi = 2020, setting = list(api_key = "dummy"), num_cores = 1, overwrite = FALSE)
  expect_true(TRUE)
})

test_that("download_planetscope_imagery runs (skipped by default)", {
  skip_on_cran()
  skip("API/long-running test not run by default")
  # Example (do not actually run):
  # download_planetscope_imagery(order_id = "dummy", exportfolder = tempdir(), api_key = "dummy", overwrite = FALSE)
  expect_true(TRUE)
}) 