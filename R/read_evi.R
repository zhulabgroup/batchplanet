read_evi <- function(dir = "alldata/PSdata/", v_site = NULL, v_taxa = NULL) {
  if (is.null(v_site)) {
    v_site <- list.files(str_c(dir, "evi/"), recursive = F, full.names = F) %>%
      str_remove(".rds") %>%
      str_split("_", simplify = T) %>%
      as.data.frame() %>%
      pull(V2)
  }

  ls_df_evi_site <- vector(mode = "list")
  for (siteoi in v_site) {
    ls_df_evi_taxa <- vector(mode = "list")
    
    if (is.null(v_taxa)) {
      v_taxa <- list.files(str_c(dir, "evi/"), recursive = F, full.names = F) %>%
        str_remove(".rds") %>%
        str_split("_", simplify = T) %>%
        as.data.frame() %>%
        pull(V3)
    }
    for (taxaoi in v_taxa) {
      f_evi <- str_c(dir, "evi/evi_", siteoi, "_", taxaoi, ".rds")
      ls_df_evi_taxa[[taxaoi]] <- read_rds(f_evi) %>%
        mutate(taxa = taxaoi)
    }
    ls_df_evi_site[[siteoi]] <- bind_rows(ls_df_evi_taxa) %>%
      mutate(site = siteoi)
  }
  df_evi <- bind_rows(ls_df_evi_site)

  return(df_evi)
}
