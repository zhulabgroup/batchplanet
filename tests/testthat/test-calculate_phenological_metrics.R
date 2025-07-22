# test-calculate_phenological_metrics.R
#
# Unit tests for calculate_phenological_metrics_batch, calculate_phenological_metrics, set_thresholds

library(testthat)
library(batchplanet)

test_that("calculate_phenological_metrics_batch runs with example data", {
  temp_dir <- withr::local_tempdir()

  # Test with example data
  data_dir <- system.file("extdata/NEON/", package = "batchplanet")
  dir.create(file.path(temp_dir, "clean"), recursive = TRUE, showWarnings = FALSE)

  file.copy(
    from = list.files(file.path(data_dir, "clean"), full.names = TRUE),
    to = file.path(temp_dir, "clean")
  )

  # Run the function
  result <- calculate_phenological_metrics_batch(
    dir = temp_dir,
    v_site = c("SJER"),
    v_group = c("Quercus"),
    v_year = c(2024),
    df_thres = set_thresholds(thres_up = 0.5, thres_down = NULL),
    var_index = "evi",
    min_days = 80,
    check_seasonality = TRUE,
    extend_to_previous_year = 275,
    extend_to_next_year = 90,
    num_cores = 1
  )

  # Expect no return
  expect_null(result)

  # Check cleaned file
  doy_files <- list.files(file.path(temp_dir, "doy"), full.names = TRUE, pattern = "doy_SJER_Quercus.rds")
  expect_gt(length(doy_files), 0)
  df_doy <- read_rds(doy_files[1])
  expect_s3_class(df_doy, "data.frame")
  expect_true(all(c("id", "start", "end", "direction", "thres", "doy") %in% names(df_doy)))
  expect_true(all(!is.na(df_doy$doy)))
})

test_that("calculate_phenological_metrics runs with example data", {
  data_dir <- system.file("extdata/NEON/", package = "batchplanet")
  df_clean <- list.files(file.path(data_dir, "clean"), full.names = TRUE, pattern = "clean_SJER_Quercus") %>%
    read_rds() %>%
    filter(year == 2024) %>%
    filter(id == "NEON.PLA.D17.SJER.06001")

  df_doy <- calculate_phenological_metrics(
    df_index = df_clean,
    df_thres = set_thresholds(thres_up = 0.5, thres_down = NULL),
    min_days = 20,
    check_seasonality = TRUE,
    var_index = "evi"
  )

  expect_s3_class(df_doy, "data.frame")
  expect_true(all(c("start", "end", "direction", "thres", "doy") %in% names(df_doy)))
  expect_true(!is.na(df_doy$doy))
})

test_that("calculate_phenological_metrics returns NULL for too few valid days", {
  df_index <- data.frame(doy = 1:365) %>%
    mutate(evi = c(rep(NA, 180), runif(10, 0.2, 0.8), c(rep(NA, 175)))) # Only 10 valid days
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
