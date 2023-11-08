vis_ts <- function(df_ts, v_site=NULL, v_taxa = NULL, v_id=NULL, v_year=NULL, n_id = 3, var = "evi", ylab= "EVI", smooth = F) {
  if (is.null(v_site)) {
    v_site <- df_ts %>% pull(site) %>% unique()
  }
  if (is.null(v_taxa)) {
    v_taxa <- df_ts %>% pull(taxa) %>% unique()
  }
  if (is.null(v_id)) {
    v_id <- df_ts %>% pull(id) %>% unique() %>% sample(min(n_id, length(.)))
  }
  if (is.null(v_year)) {
    v_year <- df_ts %>% pull(year) %>% unique()
  }

  # choose year and id
  df_ts_sub <- df_ts %>%
    filter(
      site %in% v_site,
      taxa %in% v_taxa,
      id %in% v_id,
      year %in% v_year
    ) %>% 
    mutate(value = !!sym(var))

  if (smooth) {
    df_ts_sub  <- df_ts_sub %>% 
      group_by(site, taxa,lon, lat, id, year) %>% 
      complete(doy = seq(1, 365, by = 1)) %>%
      mutate(date = lubridate::as_date(str_c(year, "-01-01"))+doy-1) %>% 
        mutate(value = util_fill_whit(x = value, maxgap = Inf, lambda = 50, minseg = 2))
  }
  # visualize
  p <- ggplot(df_ts_sub, aes(x = date, y = value, col = taxa, group = as.factor(id))) +
    geom_line() +
    labs(
      # title = paste("EVI Trend for ID:", id, "and Years:", paste(years, collapse = ", ")),
      x = "Date",
      y = ylab,
      col = "Taxa"
    ) +
    theme_minimal()

  return(p)
}
