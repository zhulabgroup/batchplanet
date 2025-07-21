#' Test for seasonality in a time series Using AIC-based model selection
#'
#' Fits a simple linear regression model and segmented regression models (with 1 to 3 breakpoints) to a numeric time series.
#' Compares their Akaike Information Criterion (AIC) values, with a penalty parameter `k`, to determine if the time series is best described as flat (no seasonality) or as having significant seasonal changes
#'
#' @param ts Numeric vector. The time series signal to evaluate (e.g., EVI, NDVI, or other index).
#' @param doy Numeric vector. The day-of-year indices corresponding to the time series. Default is `1:length(ts)`. If your `ts` is not daily, you need to provide the day-of-year indices.
#' @param k Numeric. Penalty parameter for the AIC calculation. Higher values make the function more conservative in detecting seasonality. Default is 50.
#'
#' @return Logical. Returns `TRUE` if the time series is seasonal (segmented model preferred), or `FALSE` if the time series is flat (linear model preferred).
#'
#' @examples
#' \dontrun{
#' # Simulate a seasonal time series with noise and missing values
#' t <- 1:365
#' simulate_ts <- sin(2 * pi * t / 365) + rnorm(365, sd = 0.1)
#' simulate_ts[sample(1:365, 30)] <- NA # introduce some missing data
#' 
#' # Test for seasonality
#' determine_seasonality(ts = simulate_ts, k = 50)
#'
#' # Example with a flat (non-seasonal) time series
#' flat_ts <- rnorm(365, mean = 0.5, sd = 0.05)
#' determine_seasonality(ts = flat_ts)
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
