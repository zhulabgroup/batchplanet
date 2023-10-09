set_thres <- function(taxa = NULL) {
  # possible green up and green down thresholds
  thres_up <- seq(from = 0, to = 1, by = 0.1) %>% round(1)
  thres_down <- seq(from = 1, to = 0.0, by = -0.1) %>% round(1)
  df_thres <- bind_rows(
    data.frame(direction = "up", threshold = thres_up),
    data.frame(direction = "down", threshold = thres_down)
  )

  if (!is.null(taxa)) {
    if (taxa %in% c("Ambrosia", "Ulmus late", "neon_down")) {
      df_thres <- df_thres %>% filter(direction == "down")
    } else if (taxa == "Poaceae early") {
      df_thres <- df_thres %>% filter(threshold >= 0.5 | direction == "up")
    } else if (taxa == "Poaceae late") {
      df_thres <- df_thres %>% filter(threshold >= 0.5 | direction == "down")
    } else {
      df_thres <- df_thres %>% filter(direction == "up")
    }
  }

  return(df_thres)
}
