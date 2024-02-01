set_bbox <- function(df_plant, siteoi) {
  # Set AOI (many ways to set this!) ultimately just need an extent()
  df_plant_site <- df_plant %>%
    filter(site == siteoi) %>%
    drop_na(lon, lat)
  bbox <- sf::st_bbox(c(
    xmin = min(df_plant_site$lon) - 0.0005,
    xmax = max(df_plant_site$lon) + 0.0005,
    ymin = min(df_plant_site$lat) - 0.0005,
    ymax = max(df_plant_site$lat) + 0.0005
  ))

  return(bbox)
}
