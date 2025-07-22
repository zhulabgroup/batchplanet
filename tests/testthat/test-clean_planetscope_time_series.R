# test-clean_planetscope_time_series.R
#
# Unit tests for clean_planetscope_time_series_batch and clean_planetscope_time_series

library(testthat)
library(batchplanet)

test_that("clean_planetscope_time_series_batch runs with example data", {
  temp_dir <- withr::local_tempdir()

  # Test with example data
  data_dir <- system.file("extdata/NEON/", package = "batchplanet")
  dir.create(file.path(temp_dir, "ts"), recursive = TRUE, showWarnings = FALSE)

  file.copy(
    from = list.files(file.path(data_dir, "ts"), full.names = TRUE),
    to = file.path(temp_dir, "ts")
  )

  # Run the function
  result <- clean_planetscope_time_series_batch(
    dir = temp_dir,
    v_site = c("SJER"),
    v_group = c("Quercus"),
    num_cores = 1,
    calculate_evi = TRUE
  )

  # Expect no return
  expect_null(result)

  # Check cleaned file
  clean_files <- list.files(file.path(temp_dir, "clean"), full.names = TRUE, pattern = "clean_SJER_Quercus.rds")
  expect_gt(length(clean_files), 0)
  df_clean <- read_rds(clean_files[1])
  expect_s3_class(df_clean, "data.frame")
  expect_true(all(c("id", "lon", "lat", "date", "year", "doy", "blue", "green", "red", "nir", "evi") %in% names(df_clean)))
  expect_true(all(!is.na(df_clean$evi)))
})

test_that("clean_planetscope_time_series cleans example data", {
  data_dir <- system.file("extdata/NEON/", package = "batchplanet")
  df_ts <- list.files(file.path(data_dir, "ts"), full.names = TRUE, pattern = "ts_SJER_Quercus") %>%
    read_rds()

  df_clean <- clean_planetscope_time_series(df_ts, calculate_evi = TRUE)
  expect_s3_class(df_clean, "data.frame")
  expect_true(all(c("id", "lon", "lat", "date", "year", "doy", "blue", "green", "red", "nir", "evi") %in% names(df_clean)))
  expect_true(all(!is.na(df_clean$evi)))
})
