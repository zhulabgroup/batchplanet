util_coord_in_area <- function(df_coord, sp_area, field = "NAME") {
  sp_coord <- st_as_sf(df_coord, coords = c("lon", "lat"), crs = 4326)
  indices <- st_within(sp_coord, sp_area)
  indices
  df_coord_in_area <- st_join(sp_coord, sp_area, join = st_within, left = TRUE) %>%
    cbind(st_coordinates(.)) %>%
    as_tibble() %>%
    distinct(id, date, version, lon = X, lat = Y, site = !!sym(field))

  return(df_coord_in_area)
}
