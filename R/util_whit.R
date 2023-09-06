#' Whittaker smoothing wrapper
#'
#' This function performs Whittaker smoothing based on ptw:whit1. ptw::whit1 does not take NA in the time series. This function takes a time series with NA or placeholder value (e.g., -9999). Weights at placeholder should be set to 0. NA are skipped, while values at placeholders are interpolated.
#'
#' @param x signal to be smoothed: a vector
#' @param maxgap Maximum number of successive NAs to still perform imputation on. Default setting is to replace all NAs without restrictions.
#' @param lambda smoothing parameter: larger values lead to more smoothing
#' @param minseg Minimum length of segments to perform Whittaker smoothing on. Default setting is to 2.
#'
#' @return smoothed signal: a vector
#'
#' @examples
#' # Example:
#' data.frame(
#'   doy = 1:365, # simulate some data
#'   evi = sin((1:365) / 365 * pi)
#' ) %>%
#'   filter(doy <= 20 | doy >= 40) %>% # make some short segments of data missing
#'   filter(doy <= 40 | doy >= 80) %>% # make some long segments of data missing
#'   complete(doy = seq(1, 365, by = 1)) %>%
#'   mutate(evi_sm = util_fill_whit(x = evi, maxgap = 14, lambda = 50, minseg = 2)) %>% # weighted whittaker smoothing allowing gaps %>%
#'   ggplot() +
#'   geom_line(aes(x = doy, y = evi_sm), col = "blue")
#' @export


util_fill_whit <- function(x, maxgap = Inf, lambda, minseg = 2) {
  x_fill <- imputeTS::na_replace(x, fill = -9999, maxgap = maxgap) # fill short gaps with -9999 placeholder
  w <- (x_fill != -9999) # weight = 0 at long gaps, weight = 1 at short gaps
  x_sm <- util_whit(x = x_fill, lambda = lambda, w = w, minseg = minseg)

  return(x_sm)
}


util_whit <- function(x, lambda, w, minseg = 2) {
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
      if (max_id - min_id + 1 < minseg) { # this non-NA segment is too short
        x[min_id:max_id] <- -9999
      } else {
        x[min_id:max_id] <- ptw::whit1(x[min_id:max_id], lambda, w[min_id:max_id]) # whitman smoothing for this non-NA segment
      }
    }
  }
  x[x == -9999] <- NA
  return(x)
}
