#' @export
vis_coord  <- function (df_tree) {
  p <- df_tree %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_point()+
    ggthemes::theme_few()
  
  return(p)
}