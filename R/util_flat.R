util_flat <- function(ts, doy = 1:length(ts), k = 50) {
  ls_fit <- vector(mode = "list")

  # fit simple linear regression model
  fit0 <- lm(ts ~ doy, data = data.frame(doy, ts))
  ls_fit[[1]] <- data.frame(AIC = AIC(fit0, k = k), model = "fit0")

  # fit piecewise regression model to original model with different numbers of breakpoints
  try( # sometimes fitting may fail
    {
      fit1 <- segmented::segmented(fit0, seg.Z = ~doy, npsi = 1, it = 10, control = segmented::seg.control(seed = 42, fix.npsi = T))
      ls_fit[[2]] <- data.frame(AIC = AIC(fit1, k = k), model = "fit1")
    },
    silent = T
  )

  try(
    {
      fit2 <- segmented::segmented(fit0, seg.Z = ~doy, npsi = 2, it = 10, control = segmented::seg.control(seed = 42, fix.npsi = T))
      ls_fit[[3]] <- data.frame(AIC = AIC(fit2, k = k), model = "fit2")
    },
    silent = T
  )

  try(
    {
      fit3 <- segmented::segmented(fit0, seg.Z = ~doy, npsi = 3, it = 10, control = segmented::seg.control(seed = 42, fix.npsi = T))
      ls_fit[[4]] <- data.frame(AIC = AIC(fit3, k = k), model = "fit3")
    },
    silent = T
  )

  # rank by AIC. k=50 sets a large penalty for more complex models
  df_aic <- bind_rows(ls_fit) %>%
    arrange(AIC, model)

  # return 1 when straight line is the best fit
  better <- df_aic %>%
    head(1) %>%
    pull(model) == "fit0"
  return(better)
}
