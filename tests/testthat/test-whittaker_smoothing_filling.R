# test-whittaker_smoothing_filling.R
#
# Unit tests for whittaker_smoothing_filling():
# - A perfect linear ramp remains highly correlated after smoothing.
# - Gaps longer than maxgap remain as NA.
#

library(testthat)
library(batchplanet)

test_that("Whittaker smoothing fills short gaps and smooths noise", {
  set.seed(42)
  t <- 1:100
  x <- sin(2 * pi * t / 100) + rnorm(100, sd = 0.1)
  x[sample(1:100, 10)] <- NA
  y <- whittaker_smoothing_filling(x, lambda = 20, maxgap = 5)
  expect_length(y, length(x))
  expect_true(sum(is.na(y)) < sum(is.na(x)))
})

test_that("Whittaker smoothing leaves large gaps as NA", {
  x <- c(1, NA, NA, NA, 5)
  y <- whittaker_smoothing_filling(x, lambda = 10, maxgap = 2)
  expect_true(all(is.na(y)), info = "All positions in gap > maxgap should be NA")
})
