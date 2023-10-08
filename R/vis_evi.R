library(ggplot2)

vis_evi <- function(dir, v_site, id, years) {
    # choose year and id
  df_evi <- read_rds(str_c(dir, "evi/evi_", v_site, ".rds"))
  subset_sd_evi <- df_evi[df_evi$id == id & df_evi$year %in% years, c("date", "evi")]
  
  # visualize
  cur_evi<- ggplot(subset_sd_evi, aes(x = date, y = evi)) +
    geom_line() +
    labs(title = paste("EVI Trend for ID:", id, "and Years:", paste(years, collapse = ", ")),
         x = "Date", y = "EVI Value") +
    theme_minimal()
  
  return(cur_evi)
}

