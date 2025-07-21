#' Set PlanetScope API Parameters
#'
#' Constructs a named list of parameters required for interacting with the PlanetScope API.
#'
#' @param api_key Character. API key for authentication.
#' @param item_name Character. Name of the satellite data product (default: `"PSScene"`).
#' @param asset Character. Type of asset to retrieve (default: `"ortho_analytic_4b_sr"`).
#' @param product_bundle Character. Product bundle selection (default: `"analytic_sr_udm2"`).
#' @param cloud_lim Numeric. Cloud coverage limit (between 0 and 1, default: 1).
#' @param harmonized Logical. Indicates whether to use the Planset API tool to harmonize data (default: `TRUE`).
#'
#' @return A named list containing the PlanetScope API parameters.
#'
#' @examples
#' \dontrun{
#' setting <- set_planetscope_parameters(
#'   api_key = set_api_key(),
#'   item_name = "PSScene",
#'   asset = "ortho_analytic_4b_sr",
#'   product_bundle = "analytic_sr_udm2",
#'   cloud_lim = 1,
#'   harmonized = TRUE
#' )
#' }
#'
#' @export
set_planetscope_parameters <- function(api_key,
                                       item_name = "PSScene",
                                       asset = "ortho_analytic_4b_sr",
                                       product_bundle = "analytic_sr_udm2",
                                       cloud_lim = 1,
                                       harmonized = TRUE) {
  # Validate API Key: must be a non-empty character string.
  if (missing(api_key) || !is.character(api_key) || nchar(api_key) == 0) {
    stop("Invalid API key. Please provide a valid API key.")
  }

  # Validate Cloud Limit: must be numeric and between 0 and 1.
  if (!is.numeric(cloud_lim) || cloud_lim < 0 || cloud_lim > 1) {
    stop("cloud_lim must be a numeric value between 0 and 1.")
  }

  # Helper function to validate that a parameter is a non-empty character string.
  validate_character <- function(param, param_name) {
    if (!is.character(param) || nchar(param) == 0) {
      stop(paste(param_name, "must be a non-empty character string."))
    }
  }

  validate_character(item_name, "item_name")
  validate_character(asset, "asset")
  validate_character(product_bundle, "product_bundle")

  # Validate harmonized flag: must be logical (TRUE or FALSE).
  if (!is.logical(harmonized)) {
    stop("harmonized must be a logical (TRUE or FALSE).")
  }

  # Construct and return the parameter list.
  params <- list(
    api_key = api_key,
    item_name = item_name,
    asset = asset,
    product_bundle = product_bundle,
    cloud_lim = cloud_lim,
    harmonized = harmonized
  )

  return(params)
}

#' Set or change the Planet API key
#'
#' Prompts the user to enter a Planet API key and saves it in a hidden `.env` file in the working directory.
#'
#' @param change_key Logical. If `TRUE`, prompts for a new API key even if one already exists (default: `FALSE`).
#'
#' @return Invisibly returns the API key.
#'
#' @examples
#' \dontrun{
#' set_api_key()         # Set the API key for the first time
#' set_api_key(change_key = TRUE)     # Change the API key
#' }
#'
#' @export
set_api_key <- function(change_key = F) {
  # Create .env file if it doesn't exist
  if (!file.exists(".env")) {
    file.create(".env")
  }
  dotenv::load_dot_env(".env")
  key <- Sys.getenv("planet_api_key", unset = NA) # Get env variable

  if (is.na(key) || key == "") { # If missing, prompt user
    key <- readline("Enter Planet API key: ")
    write(str_c("planet_api_key", "=", key), file = ".env", append = TRUE) # Save to .env
  } else {
    if (change_key) {
      # If the user opts to change the key, ask for a new key
      new_key <- readline("Enter new Planet API key: ")
      # Replace or add the new API key in the .env file
      key_lines <- readLines(".env")
      key_lines <- gsub("^planet_api_key=.*", str_c("planet_api_key=", new_key), key_lines)
      writeLines(key_lines, ".env")
      key <- new_key # Update the key variable
    }
  }

  invisible(key)
}

#' Set or change the data directory
#'
#' Prompts the user to enter a path to the data directory and saves it in a hidden `.env` file in the working directory.
#'
#' @param change_directory Logical. If `TRUE`, prompts for a new directory even if one already exists (default: `FALSE`).
#'
#' @return Character. The path to the data directory.
#'
#' @examples
#' \dontrun{
#' set_data_directory()         # Set the data directory for the first time
#' set_data_directory(change_directory = TRUE)     # Change the data directory
#' }
#'
#' @export
set_data_directory <- function(change_directory = F) {
  # Create .env file if it doesn't exist
  if (!file.exists(".env")) {
    file.create(".env")
  }
  dotenv::load_dot_env(".env")
  dir <- Sys.getenv("data_directory", unset = NA) # Get env variable

  if (is.na(dir) || dir == "") { # If missing, prompt user
    dir <- readline("Enter the path to the data folder: ") %>%
      normalizePath(mustWork = TRUE)
    write(str_c("data_directory", "=", dir), file = ".env", append = TRUE) # Save to .env
  } else {
    if (change_directory) {
      # If the user opts to change the dir, ask for a new dir
      new_dir <- readline("Enter the path to the new data folder: ") %>%
        normalizePath(mustWork = TRUE)
      # Replace or add the new dir in the .env file
      dir_lines <- readLines(".env")
      dir_lines <- gsub("^data_directory=.*", str_c("data_directory=", new_dir), dir_lines)
      writeLines(dir_lines, ".env")
      dir <- new_dir # Update the dir variable
    }
  }

  return(dir)
}
