#' Evaluate Flatness of a Time Series Using AIC-based Model Selection
#'
#' This function fits a simple linear regression model and segmented regression
#' models with 1 to 3 breakpoints to a given time series. It then compares their
#' Akaike Information Criterion (AIC) values (with a penalty parameter k) to assess
#' whether a straight-line (i.e., non-segmented) model is preferred. A return value
#' of TRUE indicates that the simple linear model is the best fit.
#'
#' @param ts Numeric vector. The time series signal to evaluate.
#' @param doy Numeric vector. The day-of-year indices corresponding to the time series.
#'   Default is 1:length(ts).
#' @param k Numeric. Penalty parameter for the AIC calculation. Default is 50.
#'
#' @return Logical. FALSE if the straight-line model (with no breakpoints) is preferred;
#'   TRUE otherwise.
#'
#' @examples
#' \dontrun{
#' ts <- sin(seq(0, 2 * pi, length.out = 100)) + rnorm(100, sd = 0.1)
#' is_flat <- util_flat(ts)
#' }
#'
#' @export
determine_seasonality <- function(ts, doy = 1:length(ts), k = 50) {
  ls_fit <- list()

  # Fit simple linear regression model (no breakpoints)
  fit0 <- lm(ts ~ doy, data = data.frame(doy = doy, ts = ts))
  ls_fit[[1]] <- data.frame(AIC = AIC(fit0, k = k), model = "fit0")

  # Fit segmented regression model with 1 breakpoint
  try(
    {
      fit1 <- segmented::segmented(fit0,
        seg.Z = ~doy, npsi = 1, it = 10,
        control = segmented::seg.control(seed = 42, fix.npsi = TRUE)
      )
      ls_fit[[2]] <- data.frame(AIC = AIC(fit1, k = k), model = "fit1")
    },
    silent = TRUE
  )

  # Fit segmented regression model with 2 breakpoints
  try(
    {
      fit2 <- segmented::segmented(fit0,
        seg.Z = ~doy, npsi = 2, it = 10,
        control = segmented::seg.control(seed = 42, fix.npsi = TRUE)
      )
      ls_fit[[3]] <- data.frame(AIC = AIC(fit2, k = k), model = "fit2")
    },
    silent = TRUE
  )

  # Fit segmented regression model with 3 breakpoints
  try(
    {
      fit3 <- segmented::segmented(fit0,
        seg.Z = ~doy, npsi = 3, it = 10,
        control = segmented::seg.control(seed = 42, fix.npsi = TRUE)
      )
      ls_fit[[4]] <- data.frame(AIC = AIC(fit3, k = k), model = "fit3")
    },
    silent = TRUE
  )

  # Combine the AIC results and select the model with the lowest AIC
  df_aic <- bind_rows(ls_fit) %>% arrange(AIC, model)

  # Determine if the best model is the straight-line model ("fit0")
  flat_better <- df_aic %>%
    slice(1) %>%
    pull(model) == "fit0"

  seasonal <- !flat_better

  return(seasonal)
}
