proc_evi_ts <- function(dir, v_site = NULL) {
  if (is.null(v_site)) {
    v_site <- list.files(str_c(dir, "ts/"), recursive = F, full.names = F) %>%
      str_remove(".rds") %>%
      str_remove("ps_")
  }

  dir.create(str_c(dir, "evi/"), showWarnings = F)
  for (siteoi in v_site) {
    df_ps <- read_rds(str_c(dir, "ts/ps_", siteoi, ".rds"))

    df_ps_qa <- df_ps %>%
      drop_na() %>%
      mutate(date = as.Date(time)) %>%
      mutate(
        year = format(time, "%Y") %>% as.integer(),
        doy = format(time, "%j") %>% as.integer(),
        hour = format(strptime(time, "%Y-%m-%d %H:%M:%S"), "%H") %>% as.integer()
      ) %>%
      filter(red > 0, green > 0, blue > 0) %>%
      filter(clear == 1, snow == 0, shadow == 0, haze_light == 0, haze_heavy == 0, cloud == 0, confidence >= 80) %>%
      # select(id, time, lon, lat, blue, green, red, nir) %>%
      group_by(id, lon, lat, date, year, doy) %>%
      summarise(
        blue = mean(blue),
        green = mean(green),
        red = mean(red),
        nir = mean(nir)
      ) %>%
      ungroup() %>%
      mutate(evi = 2.5 * (nir - red) / (nir + 6 * red - 7.5 * blue + 1)) %>%
      filter(evi > 0, evi <= 1)

    # # remove outliers using climatology
    # df_ps_clim <- df_ps_qa %>%
    #   group_by(doy) %>%
    #   summarise(evi_clim = median(evi, na.rm = T)) %>%
    #   ungroup() %>%
    #   complete(doy = seq(1, 365, by = 1)) %>%
    #   mutate(evi_clim_fill = zoo::na.fill(evi_clim, fill = -9999)) %>%
    #   mutate(w = (evi_clim_fill != -9999)) %>%
    #   mutate(evi_clim_sm = whitgap(x = evi_clim, lambda = 50, w = w)) %>%
    #   select(-evi_clim_fill, -w)
    #
    # df_ps_proc <- df_ps_qa %>%
    #   filter(doy <= 365) %>%
    #   left_join(df_ps_clim, by = "doy")%>%
    #   filter(abs(evi_clim_sm - evi) <= 0.2)

    write_rds(df_ps_qa, str_c(dir, "evi/evi_", siteoi, ".rds"))
  }
}
