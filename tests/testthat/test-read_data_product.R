# test-read_data_product.R
#
# Unit tests for read_data_product():
# - Reads .rds files from product_type folder.
# - Tags returned data with site and group extracted from filename.
#

library(testthat)
library(batchplanet)

test_that("read_data_product imports and annotates correctly", {
  tmp <- tempdir()
  # create product folder
  prod_dir <- file.path(tmp, "clean")
  dir.create(prod_dir, showWarnings = FALSE)
  # write a dummy RDS
  fname <- file.path(prod_dir, "clean_SiteA_GroupX.rds")
  saveRDS(data.frame(foo = 42), fname)

  df <- read_data_product(tmp, v_site = "SiteA", v_group = "GroupX", product_type = "clean")
  expect_s3_class(df, "data.frame")
  expect_equal(unique(df$site), "SiteA")
  expect_equal(unique(df$group), "GroupX")
  expect_equal(df$foo, 42)
})