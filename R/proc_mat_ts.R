proc_mat_ts <- function(dir, df_plant, v_site = NULL, v_taxa = NULL, max_sample = 2000) {
  if (is.null(v_site)) {
    v_site <- list.dirs(str_c(dir, "mat/"), recursive = F, full.names = F)
  }
  
    for (siteoi in v_site) {
      for (taxaoi in v_taxa) {
        set.seed(1)
        # get plant locations
        df_plant_site_taxa <- df_plant %>%
          filter(site == siteoi) %>%
          filter(str_detect(taxa, taxaoi)) %>%
          drop_na(lon, lat) %>%
          sample_n(min((max_sample), nrow(.)))
        
        if (nrow (df_plant_site_taxa)>0) {
          v_id <- df_plant_site_taxa %>% pull(id)
          
          # plants as points
          sf_plant_site <- sf::st_as_sf(df_plant_site_taxa,
                                        coords = c("lon", "lat"),
                                        crs = sf::st_crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
          )
          
          dir.create(str_c(dir, "ts"), showWarnings = F)
          f_ts <- str_c(dir, "ts/mat_", siteoi, "_", taxaoi, ".rds")
          if (!file.exists(f_ts)) {
            # read MAT data
            ls_year <- 2017:2022
            ls_df_ps <- vector(mode = "list")
              
              for(
              year in ls_year
            )  {
              file <- list.files(path = str_c(dir, "mat/", siteoi), pattern = year %>% as.character(),recursive = T, full.names = T)
              ras_ps <- terra::rast(file)
              
              sf_plant_site_reproj <- sf::st_transform(sf_plant_site,
                                                       crs = sf::st_crs(ras_ps)
              )
              
              ls_df_ps[[year %>% as.character()]] <- data.frame(mat = terra::extract(ras_ps, sf_plant_site_reproj) %>% select(-ID) %>% pull(ST_B10), year = year, id = v_id)
              
              print(year)
            }
            
            # join data
            df_ps <- ls_df_ps %>%
              bind_rows() %>% 
              select(year, id, everything())
            
            # save
            write_rds(df_ps, f_ts)
          }
        }
        
      }
    }
  
}
