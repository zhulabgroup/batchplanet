#' @export
proc_ps_ts <- function(dir, df_plant, v_site = NULL, v_taxa = "all", max_sample = 2000, num_cores = 36) {
  if (is.null(v_site)) {
    v_site <- list.dirs(str_c(dir, "raw/"), recursive = F, full.names = F)
  }

  cl <- makeCluster(min(num_cores, detectCores()), outfile = "")
  registerDoSNOW(cl)

  iscomplete <- F
  while (!iscomplete) { # restart when there is error, usually because of cluster connection issues
    iserror <- try({
      for (siteoi in v_site) {
        for (taxaoi in v_taxa) {
          ntif <- list.files(path = str_c(dir, "raw/", siteoi), pattern = ".tif$", recursive = T, full.names = F) %>% length()
          if (ntif > 0) {
            set.seed(1)
            # get plant locations
            df_plant_site_taxa <- df_plant %>%
              filter(site == siteoi) %>%
              filter(str_detect(taxa, if_else(taxaoi == "all", "", taxaoi))) %>%
              drop_na(lon, lat) %>%
              sample_n(min((max_sample), nrow(.)))

            if (nrow(df_plant_site_taxa) > 0) {
              v_id <- df_plant_site_taxa %>% pull(id)

              # plants as points
              sf_plant_site <- sf::st_as_sf(df_plant_site_taxa,
                coords = c("lon", "lat"),
                crs = sf::st_crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
              )

              dir.create(str_c(dir, "ts"), showWarnings = F)
              f_ts <- str_c(dir, "ts/ps_", siteoi, "_", taxaoi, ".rds")
              if (!file.exists(f_ts)) {
                # read reflectance data
                files <- list.files(path = str_c(dir, "raw/", siteoi), pattern = ".*_SR_.*clip.tif$", recursive = T, full.names = T) %>% sort()
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
                files <- list.files(path = str_c(dir, "raw/", siteoi), pattern = ".*_udm2_clip.tif$", recursive = T, full.names = T) %>% sort()
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

                # read json metadata
                meta_files <- str_replace_all(files, "_3B_udm2_clip.tif", "_metadata.json")
                nday <- length(meta_files)
                df_ps_meta <- foreach(
                  f = 1:nday,
                  .packages = c("jsonlite", "tidyverse"),
                  .combine = "rbind"
                ) %dopar% {
                  meta_file <- meta_files[f]
                  metadata <- fromJSON(meta_file)

                  df_ps_meta_f <- data.frame(
                    time = metadata$properties$acquired %>% lubridate::as_datetime(),
                    sun_elevation = metadata$properties$sun_elevation,
                    f
                  )

                  print(str_c("meta: ", f, " out of ", nday))
                  df_ps_meta_f %>% drop_na()
                }

                # assign id to each plant
                df_coord <- sf::st_coordinates(sf_plant_site) %>%
                  as_tibble() %>%
                  mutate(id = v_id) %>%
                  rename(lon = X, lat = Y)

                # join data
                df_ps_full <- df_ps %>%
                  left_join(df_ps_qa, by = c("id", "f")) %>%
                  left_join(df_ps_meta, by = "f") %>%
                  left_join(df_coord, by = "id") %>%
                  mutate(
                    red = red * 0.0001, # scaling following Dixon et al's code
                    green = green * 0.0001,
                    blue = blue * 0.0001,
                    nir = nir * 0.0001
                  ) %>%
                  select(-f) %>%
                  select(id, everything())

                # save
                write_rds(df_ps_full, f_ts)
              }
            }
          }
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
