proc_doy <- function(dir = "alldata/PSdata/", v_site = NULL, v_taxa = NULL, v_year = NULL, v_id = NULL, df_thres = NULL, min_days = 50) {
  if (is.null(v_site)) {
    v_site <- list.files(str_c(dir, "evi/"), recursive = F, full.names = F) %>%
      str_remove(".rds") %>%
      str_split("_", simplify = T) %>%
      as.data.frame() %>%
      pull(V2)
  }

  if (is.null(df_thres)) {
    df_thres <- set_thres()
  }

  dir.create(str_c(dir, "doy/"), showWarnings = F)

  for (siteoi in v_site) {
    if (is.null(v_taxa)) {
      v_taxa <- list.files(str_c(dir, "evi/"), pattern = siteoi, recursive = F, full.names = F) %>%
        str_remove(".rds") %>%
        str_split("_", simplify = T) %>%
        as.data.frame() %>%
        pull(V3)
    }

    for (taxaoi in v_taxa) {
      f_evi <- str_c(dir, "evi/evi_", siteoi, "_", taxaoi, ".rds")
      df_ts <- read_rds(f_evi)

      f_doy <- str_c(dir, "doy/doy_", siteoi, "_", taxaoi, ".rds")
      if (!file.exists(f_doy)) {
        if (nrow(df_ts) > 0) {
          if (is.null(v_year)) {
            v_year <- df_ts %>%
              pull(year) %>%
              unique() %>%
              sort()
          }
          
          cl <- makeCluster(length(v_year), outfile = "")
          registerDoSNOW(cl)
          
          ls_df_doy_year <-
            foreach(
              yearoi = v_year,
              .packages = c("tidyverse", "batchplanet") # ,
              # .export = c("v_id")
            ) %dopar% {
              if (is.null(v_id)) {
                v_id <- df_ts %>%
                  pull(id) %>%
                  unique() %>%
                  sort()
              }
              
              ls_df_doy_id <- vector(mode = "list")
              for (idoi in v_id) {
                print(str_c(yearoi, " ", idoi))
                df_ts_sub <- df_ts %>% filter(year == yearoi, id == idoi)
                ls_df_doy_id[[idoi]] <- calc_doy_ind(df_ts_ind = df_ts_sub, df_thres = df_thres, min_days = min_days) %>%
                  mutate(year = yearoi, id = idoi) %>%
                  dplyr::select(year, id, everything())
              }
              df_doy_year <- bind_rows(ls_df_doy_id)
              df_doy_year
            }
          
          df_doy <- bind_rows(ls_df_doy_year)
          stopCluster(cl)
          
          write_rds(df_doy, f_doy)
        }
      }
    }
  }
}

calc_doy_ind <- function(df_ts_ind, df_thres = NULL, min_days = 50) {
  if (is.null(df_thres)) {
    df_thres <- set_thres()
  }

  df_evi <- df_ts_ind %>%
    complete(doy = c((274 - 365):(365 + 151))) %>%
    arrange(doy) %>%
    filter(doy != 366)

  valid_days <- df_evi %>%
    drop_na() %>%
    filter(doy >= 1 & doy <= 365) %>%
    nrow()

  if (valid_days < min_days) {
    df_up <- df_down <- NULL
    print("too few data points")
  } else {
    df_evi <- df_evi %>%
      mutate(evi_sm = util_fill_whit(x = evi, maxgap = Inf, lambda = 50, minseg = 2))

    flatbetter <- util_flat(df_evi$evi_sm, k = 50)

    ### green down
    thres_list_down <- df_thres %>%
      filter(direction == "down") %>%
      pull(threshold)
    if (length(thres_list_down) == 0) {
      df_down <- NULL
    } else {
      df_evi_max <- df_evi %>%
        filter(doy >= 60 & doy <= 300) %>%
        # filter(doy >=1 & doy <=365) %>%
        arrange(desc(evi_sm), doy) %>%
        slice(1)
      max_evi <- df_evi_max$evi_sm
      start_doy <- df_evi_max$doy

      df_evi_min <- df_evi %>%
        filter(doy >= start_doy) %>%
        arrange(evi_sm, desc(doy)) %>%
        slice(1)
      min_evi <- df_evi_min$evi_sm
      end_doy <- df_evi_min$doy

      param_ok2 <- (end_doy > start_doy) & (!flatbetter)

      if (!param_ok2) {
        greendown_doy <- rep(NA, length(thres_list_down))
        start_doy <- NA
        end_doy <- NA
        print("not typical growth curve")
      } else {
        greendown_thres <- rep(NA, length(thres_list_down))
        for (t in 1:length(thres_list_down)) {
          if (thres_list_down[t] == 1) {
            greendown_thres[t] <- max_evi
          } else if (thres_list_down[t] == 0) {
            greendown_thres[t] <- min_evi
          } else {
            greendown_thres[t] <- (max_evi - min_evi) * thres_list_down[t] + min_evi
          }
        }
        greendown_thres <- (max_evi - min_evi) * thres_list_down + min_evi

        greendown_doy <- rep(NA, length(greendown_thres))
        for (t in 1:length(greendown_thres)) {
          df_evi_doy <- df_evi %>%
            filter(
              doy >= start_doy,
              doy <= end_doy
            ) %>%
            filter(evi_sm <= greendown_thres[t]) %>%
            arrange(doy) %>%
            slice(1)
          greendown_doy[t] <- df_evi_doy$doy
        }
      }
      df_down <- data.frame(start = start_doy, end = end_doy, direction = "down", thres = thres_list_down, doy = greendown_doy)
    }

    ### green up
    thres_list_up <- df_thres %>%
      filter(direction == "up") %>%
      pull(threshold)
    if (length(thres_list_up) == 0) {
      df_up <- NULL
    } else {
      df_evi_max <- df_evi %>%
        filter(doy >= 60 & doy <= 300) %>%
        # filter(doy >=1 & doy <=365) %>%
        arrange(desc(evi_sm), doy) %>%
        slice(1)
      max_evi <- df_evi_max$evi_sm
      end_doy <- df_evi_max$doy

      df_evi_min <- df_evi %>%
        filter(doy <= end_doy) %>%
        arrange(evi_sm, desc(doy)) %>%
        slice(1)
      min_evi <- df_evi_min$evi_sm
      start_doy <- df_evi_min$doy

      param_ok2 <- (end_doy > start_doy) & (!flatbetter)

      if (!param_ok2) {
        greenup_doy <- rep(NA, length(thres_list_up))
        start_doy <- NA
        end_doy <- NA
        print("not typical growth curve")
      } else {
        greenup_thres <- rep(NA, length(thres_list_up))
        for (t in 1:length(thres_list_up)) {
          if (thres_list_up[t] == 1) {
            greenup_thres[t] <- max_evi
          } else if (thres_list_up[t] == 0) {
            greenup_thres[t] <- min_evi
          } else {
            greenup_thres[t] <- (max_evi - min_evi) * thres_list_up[t] + min_evi
          }
        }

        greenup_doy <- rep(NA, length(greenup_thres))
        for (t in 1:length(greenup_thres)) {
          df_evi_doy <- df_evi %>%
            filter(
              doy >= start_doy,
              doy <= end_doy
            ) %>%
            filter(evi_sm >= greenup_thres[t]) %>%
            arrange(doy) %>%
            slice(1)
          greenup_doy[t] <- df_evi_doy$doy
        }
      }
      df_up <- data.frame(start = start_doy, end = end_doy, direction = "up", thres = thres_list_up, doy = greenup_doy)
    }
  }

  df_doy <- bind_rows(df_up, df_down)

  return(df_doy)
}
