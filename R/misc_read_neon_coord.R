misc_read_neon_coord <- function(path = "alldata/NEON/metadata.csv") {
  df <- read_csv(path) %>%
    select(site, id, lat, lon)

  return(df)
}
