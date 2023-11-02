proc_ps_ts <- function(dir, df_plant, v_site = NULL, num_cores = 36) {
  if (is.null(v_site)) {
    v_site <- list.dirs(str_c(dir, "raw/"), recursive = F, full.names = F)
  }

  cl <- makeCluster(min(num_cores, detectCores()), outfile = "")
  registerDoSNOW(cl)

  iscomplete <- F
  while (!iscomplete) { # restart when there is error, usually because of cluster connection issues
    iserror <- try({
      for (s in 1:length(v_site)) {
        # get plant locations
        siteoi <- v_site[s]
        df_plant_site <- df_plant %>%
          filter(site == siteoi) %>%
          drop_na(lon, lat)

        v_id <- df_plant_site %>% pull(id)

        # plants as points
        sf_plant_site <- sf::st_as_sf(df_plant_site,
          coords = c("lon", "lat"),
          crs = sf::st_crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
        )

        dir.create(str_c(dir, "ts"), showWarnings = F)
        if (!file.exists(str_c(dir, "ts/ps_", siteoi, ".rds"))) {
          # read reflectance data
          files <- list.files(path = str_c(dir,"raw/", siteoi), pattern = ".*_SR_clip.tif$", recursive = T, full.names = T) %>% sort()
          nday <- length(files)
          df_ps <- foreach(
            f = 1:nday,
            .packages = c("terra", "sf", "tidyverse"),
            .combine = "rbind",
            .export = c("sf_plant_site")
          ) %dopar% {
            file <- files[f]
            ras_ps <- terra::rast(file)

            sf_plant_site_reproj <- sf::st_transform(sf_plant_site,
              crs = sf::st_crs(ras_ps)
            )

            df_ps_f <- cbind(terra::extract(ras_ps, sf_plant_site_reproj) %>% select(-ID), f, id = v_id)

            print(str_c("sr: ", f, " out of ", nday))
            df_ps_f %>% drop_na()
          }

          # read quality assessment data
          # 0 - fully usable data
          # other - potentially problematic/unusable data
          #
          # Full description is in Planet's documentation (Page 91, Section 2. UNUSABLE DATA MASK FILE).
          files <- list.files(path = str_c(dir,"raw/", siteoi), pattern = ".*_udm2_clip.tif$", recursive = T, full.names = T) %>% sort()
          nday <- length(files)
          df_ps_qa <- foreach(
            f = 1:nday,
            .packages = c("terra", "sf", "tidyverse"),
            .combine = "rbind",
            .export = c("sf_plant_site")
          ) %dopar% {
            file <- files[f]
            ras_ps_qa <- terra::rast(file)

            sf_plant_site_reproj <- sf::st_transform(sf_plant_site,
              crs = sf::st_crs(ras_ps_qa)
            )

            df_ps_mask_f <- cbind(terra::extract(ras_ps_qa, sf_plant_site_reproj) %>% select(-ID), f, id = v_id)

            print(str_c("udm: ", f, " out of ", nday))
            df_ps_mask_f %>% drop_na()
          }

          # get corresponding timing from file names
          df_time <- list.files(path = str_c(dir,"raw/", siteoi), pattern = ".*_SR_clip.tif$", recursive = T) %>%
            sort() %>%
            str_split(pattern = "/", simplify = T) %>%
            data.frame() %>%
            dplyr::select(filename = X2) %>%
            rowwise() %>%
            mutate(time = strptime(str_c(str_split(filename, pattern = "_")[[1]][1], str_split(filename, pattern = "_")[[1]][2]), format = "%Y%m%d%H%M%OS")) %>%
            ungroup() %>%
            mutate(f = row_number()) %>%
            select(-filename)

          # assign id to each plant
          df_coord <- sf::st_coordinates(sf_plant_site) %>%
            as_tibble() %>%
            mutate(id = v_id) %>%
            rename(lon = X, lat = Y)

          # join data
          df_ps_full <- df_ps %>%
            left_join(df_ps_qa, by = c("id", "f")) %>%
            left_join(df_time, by = "f") %>%
            left_join(df_coord, by = "id") %>%
            mutate(
              red = red * 0.0001, # scaling following Dixon et al's code
              green = green * 0.0001,
              blue = blue * 0.0001,
              nir = nir * 0.0001
            ) %>%
            # mutate(evi=2.5* (nir-red) / (nir + 6*red - 7.5*blue + 1),
            #        gndvi=(nir-green)/(nir+green),
            #        ebi= (red + green + blue) / (green / blue * (red - blue + 1))) %>%
            select(-f) %>%
            select(id, everything())

          # save
          write_rds(df_ps_full, str_c(dir, "ts/ps_", siteoi, ".rds"))
        }
      }
    })

    if (class(iserror) != "try-error") {
      iscomplete <- T
    } else if (class(iserror) == "try-error") { # restart cluster
      iscomplete <- F
      closeAllConnections()
      cl <- makeCluster(min(num_cores, detectCores()), outfile = "")
      registerDoSNOW(cl)
    }
  }
  stopCluster(cl)
}
