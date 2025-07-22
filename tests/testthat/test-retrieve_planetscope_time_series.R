# test-retrieve_planetscope_time_series.R
#
# Unit tests for retrieve_planetscope_time_series_batch, retrieve_planetscope_time_series

library(testthat)
library(batchplanet)

test_that("retrieve_planetscope_time_series_batch works with example data", {
  temp_dir <- withr::local_tempdir()

  # Test with example data
  data_dir <- system.file("extdata/NEON/", package = "batchplanet")
  dir.create(file.path(temp_dir, "raw", "SJER", "SJER_2025_60_90"), recursive = TRUE, showWarnings = FALSE)
  file.copy(
    from = list.files(file.path(data_dir, "raw", "SJER", "SJER_2025_60_90"), full.names = TRUE),
    to = file.path(temp_dir, "raw", "SJER", "SJER_2025_60_90"),
    recursive = T
  )

  # Run the function
  df_coordinates <- read_csv(system.file("extdata", "NEON/example_neon_coordinates.csv", package = "batchplanet"), show_col_types = FALSE)

  result <- retrieve_planetscope_time_series_batch(
    dir = temp_dir,
    df_coordinates = df_coordinates,
    v_site = c("SJER"),
    v_group = c("Quercus"),
    max_sample = 10,
    num_cores = 1
  )

  # Expect no return
  expect_null(result)

  # Check cleaned file
  ts_files <- list.files(file.path(temp_dir, "ts"), full.names = TRUE, pattern = "ts_SJER_Quercus.rds")
  expect_gt(length(ts_files), 0)
  df_ts <- read_rds(ts_files[1])
  expect_s3_class(df_ts, "data.frame")
  expect_true(all(c("id", "blue", "green", "red", "nir", "time", "lon", "lat") %in% names(df_ts)))
  expect_true(all(!is.na(df_ts$green)))
})

test_that("retrieve_planetscope_time_series works with example data", {
  # Test with example data
  data_dir <- system.file("extdata/NEON/", package = "batchplanet")

  df_coordinates <- read_csv(system.file("extdata", "NEON/example_neon_coordinates.csv", package = "batchplanet"), show_col_types = FALSE)
  df_coordinates_example <- df_coordinates %>% filter(site == "SJER", group == "Quercus")

  # Run the function
  df_ts_example <- retrieve_planetscope_time_series(
    dir_site = file.path(data_dir, "raw", "SJER", "SJER_2025_60_90"),
    sf_coordinates = sf::st_as_sf(df_coordinates_example, coords = c("lon", "lat"), crs = 4326),
    num_cores = 1
  )

  expect_s3_class(df_ts_example, "data.frame")
  expect_true(all(c("id", "blue", "green", "red", "nir", "time", "lon", "lat") %in% names(df_ts_example)))
  expect_true(all(!is.na(df_ts_example$green)))
})
