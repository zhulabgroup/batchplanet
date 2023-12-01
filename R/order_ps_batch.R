order_ps_batch <- function(dir, df_plant, v_site = NULL, setting) {
  if (is.null(v_site)) {
    v_site <- df_plant$site %>% unique()
  }
  for (siteoi in v_site) {
    path_ps_site <- str_c(dir, "raw/", siteoi, "/")
    dir.create(str_c(path_ps_site, "orders/"), recursive = T, showWarnings = F)

    bbox <- set_bbox(df_plant, siteoi)

    for (year_download in 2017:2023) {
      df_order <- data.frame(year = integer(0), month = integer(0), id = character(0), images = integer(0))
      for (month_download in 1:12) {
        # Date range of interest
        start_year <- year_download
        end_year <- year_download
        date_start <- lubridate::floor_date(as.Date(paste0(year_download, "-", str_pad(month_download, 2, pad = "0"), "-01")), unit = "month")
        date_end <- lubridate::ceiling_date(as.Date(paste0(year_download, "-", str_pad(month_download, 2, pad = "0"), "-01")), unit = "month") - 1
        start_doy <- as.numeric(format(date_start, "%j"))
        end_doy <- as.numeric(format(date_end, "%j"))

        # Create order name
        order_name <- paste(siteoi, start_year, start_doy, end_doy, sep = "_")

        # Planet Orders API
        out <- tryCatch(
          {
            images <- planet_search_new(
              api_key = setting$api_key,
              bbox = bbox,
              date_start = date_start,
              date_end = date_end,
              cloud_lim = setting$cloud_lim,
              ground_control = T,
              quality = "standard",
              item_name = setting$item_name,
              asset = setting$asset
            )
          },
          error = function(e) {
            return(numeric(0))
          }
        )
        if (length(out) > 0) {
          item_num <- length(images)
          group_num <- ceiling(item_num / 450)
          split_num <- ceiling(31 / group_num)
          date_group <- split(seq(date_start, date_end, by = "day"), floor((seq(1:(end_doy - start_doy + 1)) - 0.0001) / split_num))
          for (g in 1:length(date_group)) {
            orderdone <- F
            while (!orderdone) {
              orderdone <- tryCatch(
                {
                  order_id <- planet_order_request_new(
                    api_key = setting$api_key,
                    bbox = bbox,
                    date_start = date_group[[g]] %>% min(),
                    date_end = date_group[[g]] %>% max(),
                    cloud_lim = setting$cloud_lim,
                    ground_control = T,
                    quality = "standard",
                    item_name = setting$item_name,
                    asset = setting$asset,
                    product_bundle = setting$product_bundle,
                    order_name = order_name,
                    mostrecent = 0
                  )
                  orderdone <- T
                },
                error = function(e) {
                  Sys.sleep(60)
                  print("Sleep for 60 s.")
                  return(F)
                }
              )
            }

            if (!is.null(order_id)) {
              df_order <- df_order %>%
                bind_rows(data.frame(year = year_download, month = month_download, id = order_id, images = item_num))
            }
          }
        }
        print(str_c(siteoi, ", ", year_download, ", ", month_download))
      }
      dir.create(str_c(path_ps_site, "orders/"))
      write_rds(df_order, str_c(path_ps_site, "orders/", "order_", year_download, ".rds"))
    }
  }
}
