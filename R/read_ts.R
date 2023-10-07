read_ts <- function(dir = "alldata/PSdata/", v_site = NULL) {
  if (is.null(v_site)) {
    v_site <- list.files(str_c(dir, "ts/"), recursive = F, full.names = F) %>%
      str_remove(".rds") %>%
      str_remove("ts_")
  }

  ls_df_ts <- vector(mode = "list")
  for (siteoi in v_site) {
    ls_df_ts[[siteoi]] <- read_rds(str_c(dir, "ts/ts_", siteoi, ".rds"))
  }
  df_ts <- bind_rows(ls_df_ts)

  return(df_ts)
}
