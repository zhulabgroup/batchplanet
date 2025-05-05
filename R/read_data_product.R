#' @export
read_data_product <- function(dir, v_site = NULL, v_group = NULL, product_type = "evi") {
  dir_product <- list.files(dir, pattern = product_type, recursive = F, full.names = T)

  v_file <- list.files(dir_product, recursive = FALSE, full.names = FALSE) %>%
    filter_file_names(v_site, v_group)

  ls_df <- list()
  for (file in v_file) {
    # Extract site and group from filenames
    file_parts <- file %>%
      str_remove(".rds") %>%
      str_split("_", simplify = TRUE)
    siteoi <- file_parts[, 2]
    groupoi <- file_parts[, 3]

    f <- file.path(dir_product, file)

    ls_df[[file]] <- read_rds(f) %>%
      mutate(site = siteoi) %>%
      mutate(group = groupoi)
  }
  df <- bind_rows(ls_df)

  return(df)
}


filter_file_names <- function(v_file, v_site = NULL, v_group = NULL) {
  # Extract site and group from filenames
  file_parts <- v_file %>%
    str_remove(".rds") %>%
    str_split("_", simplify = TRUE)
  sites <- file_parts[, 2]
  groups <- file_parts[, 3]

  # If site/group not provided, determine unique values from files
  if (is.null(v_site)) {
    v_site <- unique(sites)
  }
  if (is.null(v_group)) {
    v_group <- unique(groups)
  }

  # Filter files based on v_site and v_group
  keep <- sites %in% v_site & groups %in% v_group
  v_file <- v_file[keep]

  return(v_file)
}
