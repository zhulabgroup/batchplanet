# test-calculate_phenological_metrics.R
#
# Unit tests for calculate_phenological_metrics_batch, calculate_phenological_metrics, set_thresholds

library(testthat)
library(batchplanet)

test_that("calculate_phenological_metrics_batch runs (skipped by default)", {
  skip_on_cran()
  skip("Long-running or file-based test not run by default")
  # Example (do not actually run):
  # calculate_phenological_metrics_batch(dir = tempdir())
  expect_true(TRUE)
})

test_that("calculate_phenological_metrics returns NULL for too few valid days", {
  df_index <- data.frame(doy = 1:10, evi = rep(NA, 10))
  df_thres <- set_thresholds()
  result <- calculate_phenological_metrics(df_index, df_thres, min_days = 20, check_seasonality = TRUE, var_index = "evi")
  expect_null(result)
})

test_that("set_thresholds returns a data frame with up and down directions", {
  df <- set_thresholds(thres_up = c(0.3, 0.5), thres_down = c(0.7, 0.2))
  expect_s3_class(df, "data.frame")
  expect_true(all(c("direction", "threshold") %in% names(df)))
  expect_true(all(df$direction %in% c("up", "down")))
}) 