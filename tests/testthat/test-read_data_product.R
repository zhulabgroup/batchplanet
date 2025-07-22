# test-read_data_product.R
#
# Unit tests for read_data_product()

library(testthat)
library(batchplanet)

test_that("read_data_product works with multiple product types", {
  data_dir <- system.file("extdata/NEON/", package = "batchplanet")

  df_ts <- read_data_product(data_dir, product_type = "ts")
  expect_s3_class(df_ts, "data.frame")
  expect_true(all(c("id", "blue", "green", "red", "nir", "lon", "lat", "time", "site", "group") %in% names(df_ts)))
  expect_true(nrow(df_ts) > 0)

  df_clean <- read_data_product(data_dir, product_type = "clean")
  expect_s3_class(df_clean, "data.frame")
  expect_true(all(c("id", "lon", "lat", "date", "year", "doy", "blue", "green", "red", "nir", "site", "group") %in% names(df_clean)))
  expect_true(nrow(df_clean) > 0)

  df_doy <- read_data_product(data_dir, product_type = "doy")
  expect_s3_class(df_clean, "data.frame")
  expect_true(all(c("year", "id", "start", "end", "direction", "thres", "doy", "site", "group") %in% names(df_doy)))
  expect_true(nrow(df_doy) > 0)
})
