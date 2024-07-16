#' @export
read_county <- function(country = "us", dir = "alldata/juniper_ashei/") {
  if (country %>% tolower() == "us") {
    sp_county <- st_read(dsn = str_c(dir, "tl_2017_us_county/tl_2017_us_county.shp")) %>%
      st_transform(st_crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  } else {
    print("No county information for this country")
  }

  return(sp_county)
}

#' @export
read_county_sub <- function(country = "us", state = "texas", dir = "alldata/juniper_ashei/") {
  if (country %>% tolower() == "us") {
    if (state %>% tolower() == "texas") {
      sp_county_sub <- st_read(dsn = str_c(dir, "tl_2017_48_cousub/tl_2017_48_cousub.shp")) %>%
        st_transform(st_crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    } else {
      print("No county subdivision information for this state")
    }
  } else {
    print("No county information for this country")
  }

  return(sp_county_sub)
}
