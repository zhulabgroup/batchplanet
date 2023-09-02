#' Whittaker smoothing wrapper
#'
#' This function performs Whittaker smoothing based on ptw:whit1. ptw::whit1 does not take NA in the time series. This function takes a time series with NA or placeholder value (e.g., -9999). Weights at placeholder should be set to 0. NA are skipped, while values at placeholders are interpolated.
#'
#' @param x signal to be smoothed: a vector
#' @param lambda smoothing parameter: larger values lead to more smoothing
#' @param w weights: a vector of same length as y. Default weights are equal to one
#'
#' @return smoothed signal: a vector
#'
#' @examples
#' # Example:
#' data.frame(
#'   doy = 1:365, # simulate some data
#'   evi = sin(doy)
#' ) %>%
#'   filter(doy <= 20 | doy >= 40) %>% # make some data missing
#'   complete(doy = seq(1, 365, by = 1)) %>%
#'   mutate(evi_fill = zoo::na.fill(evi, fill = -9999, maxgap = 14)) %>% # fill short gaps with -9999 placeholder
#'   mutate(w = (evi_fill != -9999)) %>% # weight = 0 at long gaps, weight = 1 at short gaps
#'   mutate(evi_sm = whitgap(x = evi, lambda = 50, w = w)) %>% # weighted whittaker smoothing
#'   select(-evi_fill, -w)
#' @export


whitgap <- function(x, lambda, w) {
  max_id <- 0
  done <- F
  while (!done) {
    v_non_na <- which(!is.na(x[(max_id + 1):length(x)])) # non-NA segment
    if (length(v_non_na) == 0) { # all numbers are NA
      done <- T # consider this ts done
    } else {
      min_id <- min(v_non_na) + (max_id) # first number that is not NA
      v_na <- which(is.na(x[min_id:length(x)])) # NA segment
      if (length(v_na) == 0) { # no more NA
        max_id <- length(x) # last non-NA segment is at the end of the whole ts
        done <- T # consider this ts done
      } else {
        max_id <- min(v_na) - 1 + (min_id - 1) # index of last number in this NA segment
      }
      x[min_id:max_id] <- ptw::whit1(x[min_id:max_id], lambda, w[min_id:max_id]) # whitman smoothing for this non-NA segment
    }
  }
  return(x)
}
