# test-retrieve_planetscope_time_series.R
#
# Unit tests for retrieve_planetscope_time_series_batch, retrieve_planetscope_time_series, retrieve_planetscope_time_series_sitegroup, retrieve_raster_data

library(testthat)
library(batchplanet)

test_that("retrieve_planetscope_time_series_batch works with example data", {
  tmp <- tempdir()
  dir.create(file.path(tmp, "raw", "SiteA"), recursive = TRUE, showWarnings = FALSE)
  # Create a dummy .tif file (could use terra::rast() and terra::writeRaster() if terra is available)
  # For now, just check that the function runs with the expected structure
  df_coordinates <- data.frame(site = "SiteA", lon = 0, lat = 0, id = 1)
  expect_error(
    retrieve_planetscope_time_series_batch(tmp, df_coordinates, v_site = "SiteA", v_group = NULL, max_sample = 1, num_cores = 1),
    NA
  )
})

test_that("retrieve_planetscope_time_series works with example sf object", {
  # Create a dummy sf object
  df <- data.frame(id = 1, lon = 0, lat = 0)
  sf_obj <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
  # Use a dummy directory (no actual rasters, just check for error handling)
  expect_error(
    retrieve_planetscope_time_series(tempdir(), sf_obj, num_cores = 1),
    NA
  )
})
