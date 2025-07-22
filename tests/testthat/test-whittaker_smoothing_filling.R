# test-whittaker_smoothing_filling.R
#
# Unit tests for whittaker_smoothing_filling():
# - A perfect linear ramp remains highly correlated after smoothing.
# - Gaps longer than maxgap remain as NA.
#

library(testthat)
library(batchplanet)

test_that("Whittaker smoothing fills short gaps and smooths noise", {
  t <- 1:365

  # Double logistic function: leaf-on and leaf-off phases
  double_logistic <- function(t, L = 0.1, U = 0.6, k1 = 0.1, k2 = 0.1, t1 = 120, t2 = 280) {
    L + (U - L) * (1 / (1 + exp(-k1 * (t - t1)))) * (1 / (1 + exp(k2 * (t - t2))))
  }

  # Generate simulate_ts values using double logistic
  simulate_ts <- double_logistic(t)

  # Add Gaussian noise
  set.seed(42)
  simulate_ts <- simulate_ts + rnorm(length(t), mean = 0, sd = 0.1)

  # Introduce missing data (e.g., cloud cover)
  missing_idx <- sample(1:length(t), size = round(0.2 * length(t))) # 10% missing
  simulate_ts[missing_idx] <- NA

  smoothed_ts <- whittaker_smoothing_filling(
    x = simulate_ts,
    lambda = 50,
    maxgap = 365,
    minseg = 2
  )

  expect_length(smoothed_ts, length(t))
  expect_true(sum(is.na(smoothed_ts)) < sum(is.na(simulate_ts)))
})

test_that("Whittaker smoothing leaves large gaps as NA", {
  t <- 1:365

  # Double logistic function: leaf-on and leaf-off phases
  double_logistic <- function(t, L = 0.1, U = 0.6, k1 = 0.1, k2 = 0.1, t1 = 120, t2 = 280) {
    L + (U - L) * (1 / (1 + exp(-k1 * (t - t1)))) * (1 / (1 + exp(k2 * (t - t2))))
  }

  # Generate simulate_ts values using double logistic
  simulate_ts <- double_logistic(t)

  # Add Gaussian noise
  set.seed(42)
  simulate_ts <- simulate_ts + rnorm(length(t), mean = 0, sd = 0.1)

  # Introduce missing data (e.g., cloud cover)
  missing_idx <- 10:(length(t) - 50) # large gap in data
  simulate_ts[missing_idx] <- NA

  smoothed_ts <- whittaker_smoothing_filling(
    x = simulate_ts,
    lambda = 50,
    maxgap = 30,
    minseg = 2
  )
  expect_true(sum(is.na(smoothed_ts)) == sum(is.na(simulate_ts)))
})

test_that("Whittaker smoothing leaves tiny segments as NA", {
  t <- 1:365

  # Double logistic function: leaf-on and leaf-off phases
  double_logistic <- function(t, L = 0.1, U = 0.6, k1 = 0.1, k2 = 0.1, t1 = 120, t2 = 280) {
    L + (U - L) * (1 / (1 + exp(-k1 * (t - t1)))) * (1 / (1 + exp(k2 * (t - t2))))
  }

  # Generate simulate_ts values using double logistic
  simulate_ts <- double_logistic(t)

  # Add Gaussian noise
  set.seed(42)
  simulate_ts <- simulate_ts + rnorm(length(t), mean = 0, sd = 0.1)

  # Introduce missing data (e.g., cloud cover)
  missing_idx <- 10:(length(t) - 50) # large gap in data
  simulate_ts[missing_idx] <- NA

  # add small segment
  simulate_ts[100:102] <- rnorm(3, mean = 0.6, sd = 0.05)

  smoothed_ts <- whittaker_smoothing_filling(
    x = simulate_ts,
    lambda = 50,
    maxgap = 30,
    minseg = 8
  )
  expect_true(sum(is.na(smoothed_ts)) > sum(is.na(simulate_ts)))
})
