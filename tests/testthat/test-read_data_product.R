# test-read_data_product.R
#
# Unit tests for read_data_product():
# - Reads .rds files from product_type folder.
# - Tags returned data with site and group extracted from filename.
#

library(testthat)
library(batchplanet)

test_that("read_data_product handles multiple files and types", {
  tmp <- tempdir()
  dir.create(file.path(tmp, "clean"), showWarnings = FALSE)
  dir.create(file.path(tmp, "doy"), showWarnings = FALSE)
  saveRDS(data.frame(a = 1), file.path(tmp, "clean", "clean_SiteA_GroupX.rds"))
  saveRDS(data.frame(b = 2), file.path(tmp, "clean", "clean_SiteB_GroupY.rds"))
  saveRDS(data.frame(c = 3), file.path(tmp, "doy", "doy_SiteA_GroupX.rds"))
  df_clean <- read_data_product(tmp, v_site = "SiteA", v_group = "GroupX", product_type = "clean")
  expect_equal(df_clean$a, 1)
  df_doy <- read_data_product(tmp, v_site = "SiteA", v_group = "GroupX", product_type = "doy")
  expect_equal(df_doy$c, 3)
})
