#' @export
read_data_product <- function(dir, v_site = NULL, v_group = NULL, product_type = "evi") {
  dir_product <- list.files(dir, pattern = product_type, recursive = F, full.names = T)

  v_file <- list.files(dir_product, recursive = FALSE, full.names = FALSE)

  ls_df <- list()
  for (file in v_file) {
    # Extract site and group from filenames
    file_parts <- file %>%
      str_remove(".rds") %>%
      str_split("_", simplify = TRUE)
    siteoi <- file_parts[, 2]
    groupoi <- file_parts[, 3]

    if (!is.null(v_site)) {
      if (!siteoi %in% v_site) {
        next
      }
      if (!is.null(v_group)) {
        if (!groupoi %in% v_group) {
          next
        }
      }
    }

    f <- file.path(dir_product, file)

    ls_df[[file]] <- read_rds(f) %>%
      mutate(site = siteoi) %>%
      mutate(group = groupoi)
  }
  df <- bind_rows(ls_df)

  return(df)
}
