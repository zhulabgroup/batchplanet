# test-visualize_time_series.R
#
# Unit tests for visualize_time_series

library(testthat)
library(BatchPlanet)

test_that("visualize_time_series runs with NEON example data", {
  # Use a small subset of the NEON raw time series
  df_ts <- "sample-data/NEON/ts/ts_SJER_Quercus.rds" %>%
    read_rds() %>%
    sample_n(100)
  plt <- visualize_time_series(df_ts = df_ts, var = "green", ylab = "Green reflectance")
  expect_true("plotly" %in% class(plt))

  # Use a small subset of the NEON clean time series
  df_clean <- "sample-data/NEON/clean/clean_SJER_Quercus.rds" %>%
    read_rds() %>%
    sample_n(100)
  plt <- visualize_time_series(df_ts = df_clean, var = "evi", ylab = "EVI")
  expect_true("plotly" %in% class(plt))

  # Use a small subset of the NEON clean time series with doy
  id_example <- c("NEON.PLA.D17.SJER.06001")
  df_doy_sample <- "sample-data/NEON/doy/doy_SJER_Quercus.rds" %>%
    read_rds() %>%
    filter(id %in% id_example)
  df_evi_sample <- "sample-data/NEON/clean/clean_SJER_Quercus.rds" %>%
    read_rds() %>%
    filter(id %in% id_example)
  plt <- visualize_time_series(df_ts = df_evi_sample, df_doy = df_doy_sample, var = "evi", ylab = "EVI", smooth = T)
  expect_true("plotly" %in% class(plt))
})
