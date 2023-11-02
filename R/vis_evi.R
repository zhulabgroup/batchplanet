vis_evi <- function(df_evi, v_site=NULL, v_id=NULL, v_year=NULL) {
  if (is.null(v_site)) {
    v_site <- df_evi %>% pull(site) %>% unique()
  }
  if (is.null(v_id)) {
    v_id <- df_evi %>% pull(id) %>% unique() %>% sample(min(3, length(.)))
  }
  if (is.null(v_year)) {
    v_year <- df_evi %>% pull(year) %>% unique()
  }
  
  # choose year and id
  df_evi_sub <- df_evi %>%
    filter(
      site %in% v_site,
      id %in% v_id,
      year %in% v_year
    )

  # visualize
  cur_evi <- ggplot(df_evi_sub, aes(x = date, y = evi, col = as.factor(id))) +
    geom_line() +
    labs(
      # title = paste("EVI Trend for ID:", id, "and Years:", paste(years, collapse = ", ")),
      x = "Date",
      y = "EVI",
      col = "ID"
    ) +
    theme_minimal()

  return(cur_evi)
}
