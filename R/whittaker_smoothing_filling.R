#' Smooth and gap-fill a time series using Whittaker smoothing
#'
#' Applies weighted Whittaker smoothing to a numeric time series, filling short gaps (NAs) and smoothing the signal at the same time.
#' This function is useful for environmental time series with missing or noisy data.
#' Note that the input time series need to be sampled at a regular interval. If not, you need to resample it first, with NAs inserted at the missing time points.
#'
#' @param x Numeric vector. The time series signal to be smoothed (e.g., EVI, NDVI, or other index).
#' @param lambda Numeric. Smoothing parameter; larger values result in a smoother output.
#' @param maxgap Numeric. Maximum number of consecutive NAs to interpolate (default: Inf). Gaps longer than this will remain NA.
#' @param minseg Numeric. Minimum segment length for smoothing (default: 2). Segments shorter than this will be replaced with NA.
#'
#' @return Numeric vector. The smoothed and gap-filled signal, with the same length as `x`.
#'
#' @examples
#' \dontrun{
#' # Simulate a noisy, gappy time series
#' set.seed(42)
#' t <- 1:365
#' x <- sin(2 * pi * t / 365) + rnorm(365, sd = 0.1)
#' x[sample(1:365, 30)] <- NA
#'
#' # Smooth and fill gaps
#' x_sm <- whittaker_smoothing_filling(x, lambda = 50, maxgap = 14, minseg = 2)
#' plot(t, x, type = 'p', col = 'grey', main = 'Whittaker Smoothing')
#' lines(t, x_sm, col = 'darkgreen', lwd = 2)
#' }
#'
#' @export
whittaker_smoothing_filling <- function(x, lambda, maxgap = Inf, minseg = 2) {
  # Replace short gaps with a placeholder (-9999)
  x <- imputeTS::na_replace(x, fill = -9999, maxgap = maxgap)
  # Create a weight vector: 1 where x_fill is valid, 0 where it's the placeholder
  w <- (x != -9999)

  max_id <- 0
  done <- FALSE
  while (!done) {
    # Find indices of valid (non-NA) values starting from max_id+1
    v_non_na <- which(!is.na(x[(max_id + 1):length(x)]))
    if (length(v_non_na) == 0) {
      # No more valid values found; exit the loop
      done <- TRUE
    } else {
      min_id <- min(v_non_na) + max_id # First valid index in the current segment
      # Find indices of NA values in the remaining segment
      v_na <- which(is.na(x[min_id:length(x)]))
      if (length(v_na) == 0) {
        # No NA found; set max_id to end of vector and exit loop
        max_id <- length(x)
        done <- TRUE
      } else {
        max_id <- min(v_na) - 1 + (min_id - 1)
      }
      # If the segment length is too short, mark it with placeholder (-9999)
      if ((max_id - min_id + 1) < minseg) {
        x[min_id:max_id] <- -9999
      } else {
        # Apply Whittaker smoothing using ptw::whit1 on the valid segment
        x[min_id:max_id] <- ptw::whit1(x[min_id:max_id], lambda, w[min_id:max_id])
      }
    }
  }
  # Replace placeholder values with NA before returning
  x[x == -9999] <- NA

  return(x)
}
