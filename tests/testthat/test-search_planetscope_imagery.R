# test-search_planetscope_imagery.R
#
# Unit test for search_planetscope_imagery

library(testthat)
library(batchplanet)

test_that("search_planetscope_imagery runs (skipped by default)", {
  skip_on_cran()
  skip("API/long-running test not run by default")
  # Example (do not actually run):
  # search_planetscope_imagery(api_key = "dummy", bbox = list(xmin=0, ymin=0, xmax=1, ymax=1), date_start = "2020-01-01", date_end = "2020-01-31")
  expect_true(TRUE)
}) 