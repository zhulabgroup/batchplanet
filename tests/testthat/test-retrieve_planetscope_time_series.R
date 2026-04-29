# test-retrieve_planetscope_time_series.R
#
# Unit tests for retrieve_planetscope_time_series_batch, retrieve_planetscope_time_series

library(testthat)
library(BatchPlanet)
library(sf)

test_that("retrieve_planetscope_time_series_batch works with example data", {
  temp_dir <- withr::local_tempdir()

  # Test with example data
  data_dir <- "sample-data/NEON"
  dir.create(file.path(temp_dir, "raw", "SJER", "SJER_2025_60_90"), recursive = TRUE, showWarnings = FALSE)
  file.copy(
    from = list.files(file.path(data_dir, "raw", "SJER", "SJER_2025_60_90"), full.names = TRUE),
    to = file.path(temp_dir, "raw", "SJER", "SJER_2025_60_90"),
    recursive = T
  )

  # Run the function
  df_coordinates <- readr::read_csv(system.file("extdata", "NEON/example_neon_coordinates.csv", package = "BatchPlanet"), show_col_types = FALSE)

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
  df_ts <- readr::read_rds(ts_files[1])
  expect_s3_class(df_ts, "data.frame")
  expect_true(all(c("id", "blue", "green", "red", "nir", "time", "lon", "lat") %in% names(df_ts)))
  expect_true(all(!is.na(df_ts$green)))
})

test_that("retrieve_planetscope_time_series works with example data", {
  # Test with example data
  data_dir <- "sample-data/NEON"

  df_coordinates <- readr::read_csv(system.file("extdata", "NEON/example_neon_coordinates.csv", package = "BatchPlanet"), show_col_types = FALSE)
  df_coordinates_example <- df_coordinates |> dplyr::filter(site == "SJER", group == "Quercus")

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

test_that("remove_common_suffix correctly removes shared suffix", {
  # Basic case: clear shared suffix
  files <- c(
    "path/to/20210830_161008_07_2402_3B_AnalyticMS_SR_harmonized_clip.tif",
    "path/to/20210902_122649_104b_3B_AnalyticMS_SR_harmonized_clip.tif",
    "path/to/20210902_152245_50_2434_3B_AnalyticMS_SR_harmonized_clip.tif",
    "path/to/20210903_152004_18_2429_3B_AnalyticMS_SR_harmonized_clip.tif",
    "path/to/20210903_153132_71_2276_3B_AnalyticMS_SR_harmonized_clip.tif",
    "path/to/20210904_161121_05_2307_3B_AnalyticMS_SR_harmonized_clip.tif"
  )
  result <- remove_common_suffix(files)
  expect_equal(result, c(
    "20210830_161008_07_2402",
    "20210902_122649_104b",
    "20210902_152245_50_2434",
    "20210903_152004_18_2429",
    "20210903_153132_71_2276",
    "20210904_161121_05_2307"
  ))

  # No shared suffix: filenames returned unchanged
  no_suffix_files <- c(
    "path/to/abc.tif",
    "path/to/xyz.png"
  )
  result_no_suffix <- remove_common_suffix(no_suffix_files)
  expect_equal(result_no_suffix, c("abc.tif", "xyz.png"))

  # Output length always matches input length
  expect_equal(length(result), length(files))
})
