# test-set_planetscope_parameters.R
#
# Unit tests for set_planetscope_parameters, set_api_key, set_data_directory

library(testthat)
library(batchplanet)

test_that("set_planetscope_parameters returns a named list", {
  params <- set_planetscope_parameters(api_key = "dummy", item_name = "PSScene", asset = "ortho_analytic_4b_sr", product_bundle = "analytic_sr_udm2", cloud_lim = 1, harmonized = TRUE)
  expect_type(params, "list")
  expect_true(all(c("api_key", "item_name", "asset", "product_bundle", "cloud_lim", "harmonized") %in% names(params)))
})

test_that("set_api_key runs (skipped by default)", {
  skip_on_cran()
  skip("User input test not run by default")
  # Example (do not actually run):
  # set_api_key()
  expect_true(TRUE)
})

test_that("set_data_directory runs (skipped by default)", {
  skip_on_cran()
  skip("File system test not run by default")
  # Example (do not actually run):
  # set_data_directory("dummy_dir")
  expect_true(TRUE)
}) 