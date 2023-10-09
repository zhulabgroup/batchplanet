read_evi <- function(dir = "alldata/PSdata/", v_site = NULL) {
  if (is.null(v_site)) {
    v_site <- list.files(str_c(dir, "evi/"), recursive = F, full.names = F) %>%
      str_remove(".rds") %>%
      str_remove("evi_")
  }

  ls_df_evi <- vector(mode = "list")
  for (siteoi in v_site) {
    ls_df_evi[[siteoi]] <- read_rds(str_c(dir, "evi/evi_", siteoi, ".rds")) %>%
      mutate(site = siteoi)
  }
  df_evi <- bind_rows(ls_df_evi)

  return(df_evi)
}
