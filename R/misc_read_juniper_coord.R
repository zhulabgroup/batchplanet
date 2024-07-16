#' @export
misc_read_juniper_coord <- function(dir = "alldata/juniper_ashei/", version = NULL) {
  if (is.null(version)) {
    version <- 1:2
  }

  ls_df_tree <- vector(mode = "list")
  for (v in version) {
    if (v == 1) {
      ls_df_tree[[v]] <- read_csv(str_c(dir, "Sep22/tree_dates_coords_220930.csv")) %>%
        mutate(id = row_number() %>% as.character()) %>%
        distinct(id, lon = x, lat = y, date = sample_date) %>%
        as_tibble() %>%
        mutate(version = v)
    }

    if (v == 2) {
      ls_df_tree[[v]] <- st_read(dsn = str_c(dir, "Jan23/female_trees_only.shp")) %>%
        st_transform(st_crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) %>%
        cbind(st_coordinates(.)) %>%
        distinct(id = GlobalID, lon = X, lat = Y, date) %>%
        as_tibble() %>%
        mutate(version = v)
    }
    
    if (v ==3) {
      ls_df_tree[[v]] <- bind_rows (
        read_csv(str_c(dir, "Jan24/green_trees_latlon.csv")) %>% mutate(group = "green"),
        read_csv(str_c(dir, "Jan24/orange_trees_latlon.csv")) %>% mutate(group = "orange")) %>% 
        mutate(id = row_number() %>% as.character()) %>%
        distinct(id, lon , lat ) %>%
        as_tibble() %>%
        mutate(version = v)
    }
  }
  df_tree <- bind_rows(ls_df_tree) %>%
    mutate(taxa = "Juniper ashei")

  return(df_tree)
}
