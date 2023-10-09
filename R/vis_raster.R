vis_raster <- function(path, xmin, xmax, ymin, ymax) {
  ras_eg <- terra::rast(path)
  ras_eg_crop <- ras_eg %>% terra::crop(terra::ext(c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)))
  
  df_ras_eg <- ras_eg_crop %>%
    as.data.frame(xy = T) %>%
    as_tibble() %>%
    dplyr::select(
      b = blue,
      g = green,
      r = red,
      x,
      y
    ) %>%
    mutate(
      b = b * 0.0001,
      g = g * 0.0001,
      r = r * 0.0001
    ) %>%
    filter(
      b > 0,
      g > 0,
      r > 0
    ) %>%
    mutate(
      b = b * 5,
      g = g * 5,
      r = r * 5,
    ) %>%
    filter(
      b <= 1,
      g <= 1,
      r <= 1
    ) %>%
    mutate(rgb = rgb(r, g, b, maxColorValue = 1))
  
  p_ps_snap <- ggplot(data = df_ras_eg) +
    geom_tile(aes(x = x, y = y, fill = rgb), col = NA) +
    theme_void() +
    scale_fill_identity()
  
  return(p_ps_snap)
}