planet_search_new <- function(bbox = bbox,
                              date_end = NULL,
                              date_start = NULL,
                              cloud_lim = 0.1,
                              ground_control = TRUE,
                              quality = "standard",
                              item_name = "PSScene",
                              asset = "ortho_analytic_4b_sr",
                              api_key = "test",
                              list_dates = NULL) {
  # convert shapefile to geojson
  # shapefile of bounding box must be EPSG:4326 Projection
  geo_json_geometry <- list(
    type = jsonlite::unbox("Polygon"),
    coordinates = list(list(
      c(
        bbox$xmin,
        bbox$ymin
      ),
      c(
        bbox$xmin,
        bbox$ymax
      ),
      c(
        bbox$xmax,
        bbox$ymax
      ),
      c(
        bbox$xmax,
        bbox$ymin
      ),
      c(
        bbox$xmin,
        bbox$ymin
      )
    ))
  )

  # filter for items the overlap with our chosen geometry
  geometry_filter <- list(
    type = jsonlite::unbox("GeometryFilter"),
    field_name = jsonlite::unbox("geometry"),
    config = geo_json_geometry
  )

  # we will search for images for up to a month beforethe date we are interested in
  if (is.null(list_dates) == FALSE) {
    dategte <- paste0(min(list_dates), "T00:00:00.000Z")
    datelte <- paste0(max(list_dates), "T00:00:00.000Z")
  } else {
    dategte <- paste0(date_start, "T00:00:00.000Z")
    datelte <- paste0(date_end, "T00:00:00.000Z")
  }

  # filter images by daterange
  date_range_filter <- list(
    type = jsonlite::unbox("DateRangeFilter"),
    field_name = jsonlite::unbox("acquired"),
    config = list(
      gte = jsonlite::unbox(dategte),
      lte = jsonlite::unbox(datelte)
    )
  )


  # filter by cloud cover
  cloud_cover_filter <- list(
    type = jsonlite::unbox("RangeFilter"),
    field_name = jsonlite::unbox("cloud_cover"),
    config = list(
      lte = jsonlite::unbox(cloud_lim)
    )
  )

  # filter by ground control, quality
  gq_filter <- list(
    type = jsonlite::unbox("AndFilter"),
    config = list(
      list(
        field_name = jsonlite::unbox("ground_control"),
        type = jsonlite::unbox("StringInFilter"),
        config = list(jsonlite::unbox(tolower(ground_control)))
      ),
      list(
        field_name = jsonlite::unbox("quality_category"),
        type = jsonlite::unbox("StringInFilter"),
        config = list(jsonlite::unbox(quality))
      )
    )
  )

  # combine filters
  filter_configs <- list(
    type = jsonlite::unbox("AndFilter"),
    config = list(date_range_filter, cloud_cover_filter, gq_filter, geometry_filter) # , coverage_filter
  )

  # build request
  search_endpoint_request <- list(
    item_types = item_name,
    filter = filter_configs
  )

  # convert request to JSON
  body_json <- jsonlite::toJSON(search_endpoint_request, pretty = TRUE)

  # API request config
  url <- "https://api.planet.com/data/v1/quick-search"
  body <- body_json

  # send API request
  request <- httr::POST(url, body = body, httr::content_type_json(), httr::authenticate(api_key, ""))
  # Read first page
  res <- jsonlite::fromJSON(httr::content(request, as = "text", encoding = "UTF-8"))

  check_permission <- function(res) {
    # Check Permissions
    permissions <- do.call(rbind, lapply(1:length(res$features$`_permissions`), function(i) {
      permissions <- stringr::str_split(res$features$`_permissions`[[i]], ":", simplify = T)
      permissions <- data.frame(
        id = res$features$id[i],
        i = i,
        asset = gsub("assets.", "", permissions[, 1]),
        permission = permissions[, 2]
      )
      return(permissions)
    }))

    resDFid <- permissions[permissions$asset == asset, ]
    resDFid[resDFid$permission == "download", ]
  }

  permissions <- check_permission(res)

  # Read following pages, if exist
  while (is.null(res$`_links`$`_next`) == FALSE) {
    request <- httr::GET(httr::content(request)$`_links`$`_next`, httr::content_type_json(), httr::authenticate(api_key, ""))
    res <- jsonlite::fromJSON(httr::content(request, as = "text", encoding = "UTF-8"))
    if (is.null(unlist(res$features)) == FALSE) {
      permissions <- rbind(permissions, check_permission(res))
    }
  }

  permissions <- permissions[!is.na(permissions$id), ]

  if (unique(permissions$permission) == "download") {
    print(paste("You have DOWNLOAD permissions for these images."))

    permissions$date <- as.Date.character(permissions$id, format = "%Y%m%d")
    permissions$yday <- as.numeric(format(permissions$date, "%j"))

    if (is.null(list_dates) == FALSE) {
      permissions <- permissions[permissions$date %in% list_dates, ]
      print(paste("Found", nrow(permissions), "suitable", item_name, asset, "images that you have permission to download."))
      print(paste("In list of", length(list_dates), "dates from", min(list_dates), "to", max(list_dates)))
    } else {
      start_doy <- lubridate::yday(date_start)
      end_doy <- lubridate::yday(date_end)

      permissions <- permissions[permissions$yday >= start_doy & permissions$yday <= end_doy, ]
      print(paste("Found", nrow(permissions), "suitable", item_name, asset, "images that you have permission to download."))
      print(paste("Between yday:", start_doy, "to", end_doy))
    }

    if (nrow(permissions) > 0) {
      return(permissions$id)
    } else {
      print(paste("You DO NOT have DOWNLOAD permissions for these images. You have", toupper(unique(permissions$permission)), "permission"))
    }
  }
}



planet_order_request_new <-
  function(api_key,
           bbox,
           date_start,
           date_end,
           list_dates = NULL,
           cloud_lim,
           ground_control,
           quality,
           item_name,
           product_bundle,
           asset,
           order_name = exportfolder,
           mostrecent) {
    # SEARCH FOR IMAGES

    if (is.null(list_dates) == FALSE) {
      print("Search from date list")
      items <- planet_search_new(
        bbox = bbox,
        list_dates = list_dates,
        cloud_lim = cloud_lim,
        item_name = item_name,
        asset = asset,
        api_key = api_key
      )
    } else {
      print("Search from yday and year ranges")
      items <- planet_search_new(
        bbox = bbox,
        date_end = date_end,
        date_start = date_start,
        cloud_lim = cloud_lim,
        ground_control = ground_control,
        quality = quality,
        item_name = item_name,
        asset = asset,
        api_key = api_key,
        list_dates = list_dates
      )
    }

    if (mostrecent > 0) {
      items <- sort(items, decreasing = T)[1:mostrecent]
      print(paste("Selected", mostrecent, "most recent images."))
    }


    # ORDER API
    products <- list(
      list(
        item_ids = items,
        item_type = jsonlite::unbox(item_name),
        product_bundle = jsonlite::unbox(product_bundle)
      )
    )

    aoi <- list(
      type = jsonlite::unbox("Polygon"),
      coordinates = list(list(
        c(
          bbox$xmin,
          bbox$ymin
        ),
        c(
          bbox$xmin,
          bbox$ymax
        ),
        c(
          bbox$xmax,
          bbox$ymax
        ),
        c(
          bbox$xmax,
          bbox$ymin
        ),
        c(
          bbox$xmin,
          bbox$ymin
        )
      ))
    )

    # json structure needs specific nesting, double nested for tools hence the list(list())
    clip <- list(aoi = aoi)
    tools <- list(list(clip = clip))

    # Build request body and convert to json
    order_name <- jsonlite::unbox(order_name)
    order_body <-
      list(
        name = order_name,
        products = products,
        tools = tools
      )

    order_json <- jsonlite::toJSON(order_body, pretty = TRUE)

    url <- "https://api.planet.com/compute/ops/orders/v2"

    # Sent request (will make order, NOT REVERSIBLE, will show up on planet account)
    request <- httr::POST(url,
      body = order_json,
      httr::content_type_json(),
      username = api_key
    )

    # request content
    post_content <- httr::content(request)

    if (!is.null(post_content$field$Details[[1]]$message)) {
      print(post_content$field$Details[[1]]$message)
    }

    order_id <- post_content$id

    print(paste("Save the Order ID:", order_id))
    print("You can restart the download with `planet_order_download(order_id, order_name)`")

    return(order_id)
  }

planet_order_download_new <- function(order_id, order_name, api_key, order_num, overwrite_opt = FALSE) {
  # GET order for download
  # If you lose the order_id, don't redo the request, log onto planet and find it in the orders menu
  # order_id for example SMV2 order: "dab92990-ce3a-456c-8ad6-ca0c569b4a1a"
  url2 <- paste0("https://api.planet.com/compute/ops/orders/v2/", order_id)

  get_order <- httr::GET(
    url = url2,
    username = api_key
  )
  # Download links are in here, under _links>results>location
  get_content <- httr::content(get_order)
  # When state = 'success', ready for download

  # check if order is ready
  while (get_content$state != "success") {
    print("Order still being proccessed, trying again in 60 seconds...")
    print(get_content$state)
    Sys.sleep(60)
    get_order <- httr::GET(url = url2, username = api_key)
    get_content <- httr::content(get_order)
  }

  ## Time to download!
  print("Starting download")

  # First create download folder:
  dir.create(order_name, showWarnings = F)

  # Download each item in order
  for (i in 1:length(get_content$`_links`$results)) {
    print(paste0("Order ", order_num, ", Download: ", round(100 * (
      i / length(get_content$`_links`$results)
    ), 2), "%"))
    # find item names in order contents
    name <- get_content$`_links`$results[[i]]$name
    findslash <- gregexpr("/", name)
    startchar <- findslash[[1]][length(findslash[[1]])] + 1
    filename <- substr(name, startchar, nchar(name))

    download_url <- get_content$`_links`$results[[i]]$location

    try({
      httr::RETRY(
        "GET",
        url = download_url,
        username = api_key,
        httr::write_disk(
          path = paste(order_name, filename, sep = "/"),
          overwrite = overwrite_opt
        )
      )
    })
  }

  print(paste0("Download complete"))
  print(paste0("Items located in ", order_name))
}
