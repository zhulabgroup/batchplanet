read_doy <- function(dir = "alldata/PSdata/doy/", v_site = NULL, v_taxa = NULL) {
  if (is.null(v_site)) {
    v_site <- list.files(dir, recursive = F, full.names = F) %>%
      str_remove(".rds") %>%
      str_split("_", simplify = T) %>%
      as.data.frame() %>%
      pull(V2)
  }

  ls_df_doy_site <- vector(mode = "list")
  for (siteoi in v_site) {
    ls_df_doy_taxa <- vector(mode = "list")

    if (is.null(v_taxa)) {
      v_taxa <- list.files(dir, recursive = F, full.names = F) %>%
        str_remove(".rds") %>%
        str_split("_", simplify = T) %>%
        as.data.frame() %>%
        pull(V3)
    }
    for (taxaoi in v_taxa) {
      f_doy <- list.files(dir, pattern = str_c(siteoi, "_", taxaoi), full.names = T)
      ls_df_doy_taxa[[taxaoi]] <- read_rds(f_doy) %>%
        mutate(date = lubridate::as_date(str_c(year, "-01-01")) + doy - 1) %>%
        mutate(taxa = taxaoi)
    }
    ls_df_doy_site[[siteoi]] <- bind_rows(ls_df_doy_taxa) %>%
      mutate(site = siteoi)
  }
  df_doy <- bind_rows(ls_df_doy_site)

  return(df_doy)
}
