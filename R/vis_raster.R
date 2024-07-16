#' @export
vis_raster <- function(path, crop_shape = NULL, bright = 5) {
  ras <- terra::rast(path)
  if (!is.null(crop_shape)) {
    ras <- ras %>% terra::crop(crop_shape)
  }

  # crop_shape <- terra::ext(c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)

  df_ras <- ras %>%
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
      b = b * bright,
      g = g * bright,
      r = r * bright,
    ) %>%
    filter(
      b <= 1,
      g <= 1,
      r <= 1
    ) %>%
    mutate(rgb = rgb(r, g, b, maxColorValue = 1))

  p_ps_snap <- ggplot(data = df_ras) +
    geom_tile(aes(x = x, y = y, fill = rgb), col = NA) +
    theme_void() +
    scale_fill_identity()

  return(p_ps_snap)
}
