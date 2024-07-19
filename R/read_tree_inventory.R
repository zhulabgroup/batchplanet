#' @export
read_tree_inventory_all <- function(v_site, indir = "alldata/tree/") {
  ls_df_tree <- vector(mode = "list")
  for (siteoi in v_site) {
    ls_df_tree[[siteoi]] <- read_tree_inventory(siteoi, indir = indir)
    print(siteoi)
  }

  df_tree <- bind_rows(ls_df_tree)

  f_tree <- str_c(indir, "df_tree.rds")
  write_rds(df_tree, f_tree)

  return(f_tree)
}

#' @export
read_tree_inventory <- function(city, indir = "alldata/tree/", preload = F) {
  if (!preload) {
    read_tree_inventory_city <- str_c("read_tree_inventory_", city)
    df_tree <- do.call(read_tree_inventory_city, list(indir = indir, site = city)) %>%
      rename(taxa = species)
  }
  if (preload) {
    f_tree <- str_c(indir, "df_tree.rds")
    df_tree <- read_rds(f_tree) %>%
      filter(site == city)
  }

  return(df_tree)
}

read_tree_inventory_DT <- function(indir, site) {
  # Data requested from DT P&R, shared by Dan Katz
  trees_df <- st_read(dsn = str_c(indir, "Tree_Inventory_Detroit/dvy46298.shp")) %>%
    st_set_crs(st_crs("+proj=lcc +lat_0=41.5 +lon_0=-84.3666666666667 +lat_1=42.1 +lat_2=43.6666666666667 +x_0=4000000 +y_0=0 +ellps=GRS80 +units=ft +no_defs")) %>%
    st_transform(st_crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) %>%
    cbind(st_coordinates(.)) %>%
    distinct(id = ID, species = SPP, lon = X, lat = Y) %>%
    mutate(site = site)
  # ESRI:102290: NAD 1983 HARN StatePlane Michigan South FIPS 2113
  # https://spatialreference.org/ref/?search=michigan

  return(trees_df)
}
read_tree_inventory_DV <- function(indir, site) {
  # Data from OpenTrees.org
  trees_df <- read_csv(str_c(indir, "Tree_Inventory_Denver.csv")) %>%
    dplyr::select(id = SITE_ID, species = SPECIES_BO, time = INVENTORY_DATE, lat = Y_LAT, lon = X_LONG) %>%
    arrange(desc(time)) %>%
    distinct(id, species, .keep_all = T) %>%
    dplyr::select(-time) %>%
    filter(lon != 0) %>%
    mutate(site = site)

  return(trees_df)
}
read_tree_inventory_TP <- function(indir, site) {
  # Data from https://www.opentreemap.org/tampa/map/
  trees_df <- read_csv(str_c(indir, "Tree_Inventory_Tampa.csv")) %>%
    dplyr::select(id = `Tree Id`, genus = Genus, species = Species, lat = `Point Y`, lon = `Point X`) %>% # checked that there is no repeated tree id
    mutate(species = paste0(genus, " ", species)) %>%
    dplyr::select(-genus) %>%
    mutate(site = site)

  return(trees_df)
}
read_tree_inventory_HT <- function(indir, site) {
  # Data from  https://koordinates.com/layer/25245-houston-texas-street-tree-inventory/data/
  trees_df <- read_csv(str_c(indir, "Tree_Inventory_Houston/houston-texas-street-tree-inventory.csv")) %>%
    dplyr::select(species_common = Species, X = Shape_X, Y = Shape_Y) %>%
    rowwise() %>%
    mutate(common = case_when(
      str_detect(species_common, ", spp.") ~ str_replace(species_common, ", spp.", ""), # for coarse common names, e.g., "Oak, spp."
      (str_detect(species_common, ", ") & !str_detect(species_common, ", spp.")) ~ paste0(str_split(species_common, ", ")[[1]][2], " ", str_split(species_common, ", ")[[1]][1]), # reverse order of common name, e.g., "Oak, Water"
      TRUE ~ species_common
    )) %>%
    ungroup() %>%
    distinct(X, Y, .keep_all = T) %>% # in case same tree is sampled repeatedly
    mutate(id = row_number())

  # reproject to WGS84
  pts <- SpatialPoints(trees_df[, c("X", "Y")],
    proj4string = CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs")
  )
  pts_reproj <- spTransform(
    pts,
    CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  )

  trees_df <- cbind(trees_df %>% dplyr::select(-X, -Y), coordinates(pts_reproj)) %>%
    rename(lat = Y, lon = X)

  # Use taxize to match common name with scientific name
  if (!file.exists(str_c(indir, "Tree_Inventory_Houston/species_reference.csv"))) {
    id2comm <- function(pageid) {
      common_matches <- eol_pages(taxonconceptID = pageid, common_names = TRUE)$vernacular
      if (is.null(common_matches)) {
        common_match <- ""
      } else {
        common_matches <- common_matches %>% filter(language == "en")
        if (nrow(common_matches) > 0) {
          common_match <- common_matches %>%
            pull(vernacularname) %>%
            table() %>%
            sort(decreasing = TRUE) %>%
            head(1) %>%
            names()
        }
      }
      return(common_match)
    }

    comm2sci_new <- function(common) {
      species <- comm2sci(common, simplify = T)[[1]]
      if (length(species) == 0) {
        res <- comm2sci(common, db = "eol", simplify = F)[[1]]
        if (length(res) == 0) {
          species <- NA
        } else {
          species <- res %>%
            as_tibble() %>%
            rowwise() %>%
            mutate(common_match = id2comm(pageid)) %>%
            ungroup() %>%
            stringdist_inner_join(data.frame(common_name = common), by = c("common_match" = "common_name"), max_dist = 20, distance_col = "distance") %>%
            arrange(distance, pageid) %>%
            head(1) %>%
            filter(common_match != "") %>%
            pull(name)
          if (length(species) == 0) {
            species <- NA
          }
        }
        print(paste(common, species))
      }

      if (is.na(species)) {
        species <- readline(prompt = paste0(species_df$common[i], ". Manually enter scientific name: "))
      }

      return(species)
    }

    species_df <- trees_df %>%
      distinct(common) %>%
      rowwise() %>%
      mutate(species = comm2sci_new(common))
    write_csv(species_df, str_c(indir, "Tree_Inventory_Houston/species_reference.csv"))
  } else {
    species_df <- read_csv(str_c(indir, "Tree_Inventory_Houston/species_reference.csv"))
  }

  trees_df <- trees_df %>%
    left_join(species_df, by = "common") %>%
    dplyr::select(-species_common, -common) %>%
    mutate(site = site)

  return(trees_df)
}
read_tree_inventory_NY <- function(indir, site) {
  trees_df <- read_csv(str_c(indir, "Tree_Inventory_NewYork.csv")) %>%
    dplyr::select(id = tree_id, species = spc_latin, lat = latitude, lon = longitude) %>% # checked that there is no repeated tree id
    mutate(site = site)

  return(trees_df)
}
read_tree_inventory_AT <- function(indir, site) {
  trees_df <- read_csv(str_c(indir, "Tree_Inventory_Austin/Tree_Inventory_Austin.csv")) %>%
    dplyr::select(id = OBJECTID, species = SPECIES, coordinates = the_geom) %>% # checked that there is no repeated tree id
    mutate(coordinates = str_replace(coordinates, "POINT \\(", "")) %>%
    mutate(coordinates = str_replace(coordinates, "\\)", "")) %>%
    rowwise() %>%
    mutate(
      lon = str_split(coordinates, pattern = " ", simplify = T)[1],
      lat = str_split(coordinates, pattern = " ", simplify = T)[2]
    ) %>%
    ungroup() %>%
    mutate(
      lon = as.numeric(lon),
      lat = as.numeric(lat)
    ) %>%
    dplyr::select(-coordinates) %>%
    mutate(site = site)

  return(trees_df)
}
read_tree_inventory_SJ <- function(indir, site) {
  trees_df <- read_csv(str_c(indir, "Tree_Inventory_SanJose/Tree_Inventory_SanJose.csv")) %>%
    dplyr::select(id = OBJECTID, species = NAMESCIENTIFIC, Y = Y, X = X) # checked that there is no repeated tree id

  # shape <- readOGR(dsn = "/data/ZHULAB/phenology/occurrence/StreetTrees/Tree_Inventory_SanJose/Street_Tree.shp")
  # proj4string(shape)
  projection_sj <- "+proj=lcc +lat_0=36.5 +lon_0=-120.5 +lat_1=38.4333333333333 +lat_2=37.0666666666667 +x_0=2000000.0001016 +y_0=500000.0001016 +datum=NAD83 +units=us-ft +no_defs"
  pts <- SpatialPoints(trees_df[, c("X", "Y")],
    proj4string = CRS(projection_sj)
  )
  pts_reproj <- spTransform(
    pts,
    CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  )

  trees_df <- cbind(trees_df %>% dplyr::select(-X, -Y), coordinates(pts_reproj)) %>%
    rename(lat = Y, lon = X) %>%
    mutate(site = site)

  return(trees_df)
}
read_tree_inventory_ST <- function(indir, site) {
  trees_df <- read_csv(str_c(indir, "Tree_Inventory_Seattle.csv")) %>%
    dplyr::select(id = OBJECTID, species = SCIENTIFIC_NAME, lat = Y, lon = X) %>% # checked that there is no repeated tree id
    mutate(site = site)

  return(trees_df)
}

read_tree_inventory_BV <- function(indir, site) {
  # Source?
  species_name <- read_csv(str_c(indir, "Tree_Inventory_Bellevue/species_reference.csv"))
  trees_df <- st_read(dsn = str_c(indir, "Tree_Inventory_Bellevue/CITYTREE.shp")) %>%
    st_transform(shapefile, crs = 4326) %>%
    cbind(st_coordinates(.)) %>%
    left_join(species_name, c("SpeciesNbr" = "species_id")) %>%
    distinct(id = CityTreeID, species = species, lon = X, lat = Y) %>%
    filter(!is.na(species)) %>%
    mutate(site = site)
  # original crs: NAD83(2011) / Washington North (ftUS)
  # remove NA species_name - reduce from 11725 to 4252
  return(trees_df)
}

read_tree_inventory_SL <- function(indir, site) {
  # Source?
  add_df <- read_csv(str_c(indir, "Tree_Inventory_Salem/address reference.csv"))
  trees_df <- read_csv(str_c(indir, "Tree_Inventory_Salem/Tree_Inventory_Salem.csv")) %>%
    bind_cols(add_df)
  # check <- trees_df %>%
  #   mutate(
  #     extracted_number = as.numeric(sub(" .*", "", address)),
  #     extracted_street = sub("^[0-9]+\\s+(.*?)\\s+Salem, MA$", "\\1", address)
  #   ) %>%
  #   mutate(
  #     number_match = Street_Number == extracted_number,
  #     street_match = Street_Name == extracted_street
  #   ) %>%
  #   subset(number_match==FALSE | street_match==FALSE)
  trees_df <- trees_df %>% # check
    filter(!(is.na(Genus) & is.na(Species))) %>%
    filter(!(is.na(lon) | is.na(lat))) %>%
    mutate(species = paste(Genus, Species, sep = " ")) %>%
    distinct(species = species, lon = lon, lat = lat) %>%
    mutate(id = row_number(), site = site)
  # original crs: NAD83(2011) / Washington North (ftUS)
  # remove NA species_name - reduce from 11725 to 4252
  return(trees_df)
}

read_tree_inventory_BM <- function(indir, site) {
  # Opentree
  trees_df <- read_csv(str_c(indir, "Tree_Inventory_Bozeman.csv")) %>%
    filter(!(is.na(Genus) & is.na(Species))) %>%
    dplyr::select(id = OBJECTID, species = Botanical_Name, lat = Y, lon = X) %>% # checked that there is no repeated tree id
    mutate(site = site)

  return(trees_df)
}
