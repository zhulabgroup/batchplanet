#' Whittaker Smoothing Wrapper
#'
#' Applies Whittaker smoothing to a time series. This function handles NA values and
#' placeholder values (-9999) by interpolating over short gaps.
#'
#' @param x Numeric vector. The time series signal to be smoothed.
#' @param lambda Numeric. Smoothing parameter; larger values result in a smoother output.
#' @param maxgap Numeric. Maximum number of consecutive NAs to interpolate (default: Inf).
#' @param minseg Numeric. Minimum segment length for smoothing (default: 2).
#'
#' @return A numeric vector containing the smoothed signal.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(doy = 1:365, evi = sin((1:365) / 365 * pi))
#' df <- df %>%
#'   filter(doy <= 20 | doy >= 40) %>%
#'   complete(doy = seq(1, 365, by = 1))
#' df <- df %>% mutate(evi_sm = util_fill_whit(x = evi, maxgap = 14, lambda = 50, minseg = 2))
#' ggplot(df, aes(x = doy, y = evi_sm)) +
#'   geom_line(col = "blue")
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
