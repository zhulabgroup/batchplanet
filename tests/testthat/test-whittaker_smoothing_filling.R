# test-whittaker_smoothing_filling.R
#
# Unit tests for whittaker_smoothing_filling():
# - A perfect linear ramp remains highly correlated after smoothing.
# - Gaps longer than maxgap remain as NA.
#

library(testthat)
library(batchplanet)

test_that("Whittaker smoothing preserves linear trend", {
  x <- 1:10
  y <- whittaker_smoothing_filling(x, lambda = 10)
  expect_length(y, length(x))
  expect_type(y, "double")
  expect_gt(cor(x, y), 0.99)
})

test_that("Whittaker smoothing leaves large gaps as NA", {
  x <- c(1, NA, NA, NA, 5)
  y <- whittaker_smoothing_filling(x, lambda = 10, maxgap = 2)
  expect_true(all(is.na(y)), info = "All positions in gap > maxgap should be NA")
})
