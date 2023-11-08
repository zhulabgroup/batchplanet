vis_doy <- function(df_evi, df_doy, v_site = NULL, v_taxa = NULL, v_id = NULL, v_year = NULL, smooth = T, n_id = 10, thresoi = 0.5, directionoi = "up") {
  if (is.null(v_site)) {
    v_site <- df_doy %>%
      pull(site) %>%
      unique()
  }
  if (is.null(v_taxa)) {
    v_taxa <- df_doy %>%
      pull(taxa) %>%
      unique()
  }
  if (is.null(v_id)) {
    v_id <- df_doy %>%
      pull(id) %>%
      unique() %>%
      sample(min(n_id, length(.)))
  }
  if (is.null(v_year)) {
    v_year <- df_doy %>%
      pull(year) %>%
      unique()
  }

  # choose year and id
  df_doy_sub <- df_doy %>%
    filter(
      site %in% v_site,
      taxa %in% v_taxa,
      id %in% v_id,
      year %in% v_year,
      direction == directionoi,
      thres == thresoi
    ) %>%
    mutate(date = lubridate::as_date(str_c(year, "-01-01")) + doy - 1)

  # choose year and id
  df_evi_sub <- df_evi %>%
    filter(
      site %in% v_site,
      taxa %in% v_taxa,
      id %in% v_id,
      year %in% v_year
    ) %>%
    mutate(value = evi)

  if (smooth) {
    df_evi_sub <- df_evi_sub %>%
      group_by(site, taxa, lon, lat, id, year) %>%
      complete(doy = seq(1, 365, by = 1)) %>%
      mutate(date = lubridate::as_date(str_c(year, "-01-01")) + doy - 1) %>%
      mutate(value = util_fill_whit(x = value, maxgap = Inf, lambda = 50, minseg = 2))
  }

  # visualize
  p <- ggplot() +
    geom_line(data = df_evi_sub, aes(x = date, y = value, group = as.factor(id))) +
    geom_vline(data = df_doy_sub, aes(xintercept = date), col = "dark green") +
    xlim(min(df_doy_sub$date, na.rm = T), max(df_doy_sub$date, na.rm = T)) +
    ylim(0, 1) +
    labs(
      # title = paste("EVI Trend for ID:", id, "and Years:", paste(years, collapse = ", ")),
      x = "Date",
      col = "Taxa"
    ) +
    facet_wrap(. ~ id) +
    theme_classic()

  return(p)
}
