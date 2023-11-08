down_ps_batch <- function(dir, v_site = NULL, setting) {
  if (is.null(v_site)) {
    v_site <- list.dirs(str_c(dir, "raw/"), recursive = F, full.names = F)
  }
  for (siteoi in v_site) {
    path_ps_site <- str_c(dir, "raw/", siteoi, "/")
    for (year_download in 2017:2022) {
      df_order <- read_rds(str_c(path_ps_site, "orders/", "order_", year_download, ".rds"))
      cl <- makeCluster(nrow(df_order), outfile = "")
      registerDoSNOW(cl)
      foreach(
        i = 1:nrow(df_order),
        .packages = c("lubridate", "stringr", "httr", "batchplanet")
      ) %dopar% {
        # Get order id
        month_download <- df_order$month[i]
        order_id <- df_order$id[i]

        # Date range of interest
        start_year <- year_download
        end_year <- year_download
        date_start <- lubridate::floor_date(as.Date(str_c(year_download, "-", str_pad(month_download, 2, pad = "0"), "-01")), unit = "month")
        date_end <- lubridate::ceiling_date(as.Date(str_c(year_download, "-", str_pad(month_download, 2, pad = "0"), "-01")), unit = "month") - 1
        start_doy <- as.numeric(format(date_start, "%j"))
        end_doy <- as.numeric(format(date_end, "%j"))

        # Set/Create Export Folder
        order_name <- paste(siteoi, start_year, start_doy, end_doy, sep = "_")
        exportfolder <- str_c(path_ps_site, order_name)
        dir.create(exportfolder, recursive = T, showWarnings = F)

        # Download
        Sys.sleep(i * 0.5) # Otherwise sending request to API at the same time may cause error
        orderdone <- F
        while (!orderdone) {
          orderdone <- tryCatch(
            {
              planet_order_download_new(order_id, exportfolder, api_key = setting$api_key, order_num = i, overwrite_opt = FALSE)
              orderdone <- T
            },
            error = function(e) {
              Sys.sleep(10)
              print("Sleep for 10 s.")
              return(F)
            }
          )
        }

        print(str_c(siteoi, ", ", year_download, ", ", month_download))
      }

      stopCluster(cl)
    }
  }
}
