misc_read_juniper_coord <- function(dir = "alldata/juniper_ashei/", version = NULL) {
  if (is.null(version)) {
    version <- 1:2
  }

  ls_df_tree <- vector(mode = "list")
  for (v in version) {
    if (v == 1) {
      ls_df_tree[[v]] <- read_csv(str_c(dir, "tree_dates_coords_220930.csv")) %>%
        mutate(id = row_number() %>% as.character()) %>%
        distinct(id, lon = x, lat = y, date = sample_date) %>%
        as_tibble() %>%
        mutate(version = v)
    }

    if (v == 2) {
      ls_df_tree[[v]] <- st_read(dsn = str_c(dir, "DK_TX_jan23/female_trees_only.shp")) %>%
        st_transform(st_crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) %>%
        cbind(st_coordinates(.)) %>%
        distinct(id = GlobalID, lon = X, lat = Y, date) %>%
        as_tibble() %>%
        mutate(version = v)
    }
  }
  df_tree <- bind_rows(ls_df_tree)

  return(df_tree)
}
