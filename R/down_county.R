#' @export
down_county <- function(dir = "alldata/juniper_ashei/") {
  url <- "https://www2.census.gov/geo/tiger/TIGER2017/COUNTY/tl_2017_us_county.zip"
  destfile <- str_c(dir, "tl_2017_us_county.zip")
  destdir <- str_c(dir, "tl_2017_us_county/")

  download.file(url, destfile)

  unzip(destfile, exdir = destdir)

  return(destdir)
}
