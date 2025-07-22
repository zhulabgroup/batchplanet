# test-order_planetscope_imagery.R
#
# Unit tests for order_planetscope_imagery_batch, order_planetscope_imagery, set_bbox

library(testthat)
library(batchplanet)

test_that("order_planetscope_imagery_batch runs (skipped by default)", {
  skip_on_cran()
  skip("API/long-running test not run by default")
  # Example (do not actually run):
  # order_planetscope_imagery_batch(dir = tempdir(), df_coordinates = data.frame())
  expect_true(TRUE)
})

test_that("order_planetscope_imagery runs (skipped by default)", {
  skip_on_cran()
  skip("API/long-running test not run by default")
  # Example (do not actually run):
  # order_planetscope_imagery(api_key = "dummy", bbox = list(xmin=0, ymin=0, xmax=1, ymax=1), items = c("id1"), item_name = "PSScene", product_bundle = "analytic_sr_udm2", harmonized = TRUE, order_name = "test", mostrecent = 0)
  expect_true(TRUE)
})

test_that("set_bbox returns a bbox for dummy data", {
  df_coords <- data.frame(site = c("A", "A"), lon = c(0, 1), lat = c(0, 1))
  bbox <- set_bbox(df_coords, "A", buffer = 0.001)
  expect_true(is.list(bbox) || inherits(bbox, "bbox"))
})
