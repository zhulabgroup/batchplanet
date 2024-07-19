#' @export
proc_evi_ts <- function(dir, v_site = NULL, v_taxa = NULL) {
  if (is.null(v_site)) {
    v_site <- list.files(str_c(dir, "ts/"), recursive = F, full.names = F) %>%
      str_remove(".rds$") %>%
      str_split("_", simplify = T) %>%
      as.data.frame() %>%
      pull(V2)
  }

  dir.create(str_c(dir, "evi/"), showWarnings = F)
  for (siteoi in v_site) {
    if (is.null(v_taxa)) {
      v_taxa <- list.files(str_c(dir, "ts/"), pattern = siteoi, recursive = F, full.names = F) %>%
        str_remove(".rds$") %>%
        str_split("_", simplify = T) %>%
        as.data.frame() %>%
        pull(V3)
    }
    for (taxaoi in v_taxa) {
      f_ts <- str_c(dir, "ts/ps_", siteoi, "_", taxaoi, ".rds")
      df_ps <- read_rds(f_ts)

      df_ps_qa <- df_ps %>%
        drop_na() %>%
        mutate(date = as.Date(time)) %>%
        mutate(
          year = format(time, "%Y") %>% as.integer(),
          doy = format(time, "%j") %>% as.integer(),
          hour = format(strptime(time, "%Y-%m-%d %H:%M:%S"), "%H") %>% as.integer()
        ) %>%
        filter(sun_elevation > 0) %>% # remove night time images, but there should not be any in this data product
        filter(red > 0, green > 0, blue > 0) %>%
        filter(clear == 1, snow == 0, shadow == 0, haze_light == 0, haze_heavy == 0, cloud == 0, confidence >= 80) %>%
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

      f_evi <- str_c(dir, "evi/evi_", siteoi, "_", taxaoi, ".rds")
      write_rds(df_ps_qa, f_evi)
    }
  }
}
