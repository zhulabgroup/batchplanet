# test-determine_seasonality.R
#
# Unit tests for determine_seasonality()
library(testthat)
library(batchplanet)

test_that("Flat series is not seasonal", {
  ts_flat <- seq(0.4, 0.5, length.out = 365)
  result <- suppressWarnings(determine_seasonality(ts_flat))
  expect_false(result)
})

test_that("Simulated double logistic series is seasonal", {
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

  result <- suppressWarnings(determine_seasonality(simulate_ts))
  expect_true(result)
})
