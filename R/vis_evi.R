vis_evi <- function(df_evi, v_site,v_taxa, v_id, v_year) {
  # choose year and id
  df_evi_sub <- df_evi %>%
    filter(
      site %in% v_site,
      taxa %in% v_taxa,
      id %in% v_id,
      year %in% v_year
    )

  # visualize
  cur_evi <- ggplot(df_evi_sub, aes(x = date, y = evi, col = taxa, group = as.factor(id))) +
    geom_line() +
    labs(
      # title = paste("EVI Trend for ID:", id, "and Years:", paste(years, collapse = ", ")),
      x = "Date",
      y = "EVI",
      col = "Taxa"
    ) +
    theme_minimal()

  return(cur_evi)
}
