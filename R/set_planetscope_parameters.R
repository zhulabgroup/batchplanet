#' Set PlanetScope API Parameters
#'
#' Constructs a named list of parameters required for interacting with the PlanetScope API.
#'
#' @param api_key Character. API key for authentication. We recommend using \code{\link{set_api_key}()} to save your API key in a hidden .env file.
#' @param item_name Character. Name of the satellite data product (default: `"PSScene"`).
#' @param asset Character. Type of asset to retrieve (default: `"ortho_analytic_4b_sr"`).
#' @param product_bundle Character. Product bundle selection (default: `"analytic_sr_udm2"`).
#' @param cloud_lim Numeric. Cloud coverage limit (between 0 and 1). Images with a cloud coverage above this fraction will be ignored. (default: 1).
#' @param harmonized Logical. Indicates whether to use the Planset API tool to harmonize data with Sentinel-2 (default: `TRUE`).
#'
#' @details You will need an active Planet account and an API key to access the PlanetScope API. You can sign up for an account on the \href{https://www.planet.com/get-started/}{Planet website}. Once you have an account, you can copy your API key from your account settings.
#' @details An __item_type__ represents an imagery product. The default item type is "PSScene", which is PlanetScope 3, 4, and 8 band scenes captured by the Dove satellite constellation. Other item types include "TanagerScene", "TanagerMethane", "REOrthoTile", "REScene", "SkySatScene", "SkySatCollect", and "SkySatVideo". Please refer to \href{https://docs.planet.com/data/imagery/}{the Planet catalog} for the most updated available item types.
#' @details An __asset__ is a product derived from the item's source data, and can be used for various analytic, visual or other purposes. For example, a full list of available assets for "PSScene" can be found \href{https://docs.planet.com/data/imagery/planetscope/psscene/}{here}. Please visit \href{https://docs.planet.com/data/imagery/}{the Planet catalog} for a full list of available assets for each item type.
#' @details A __product bundle__ comprises of a group of assets for an item. This is often useful when you want to download associated metadata and data quality masks. A full list of bundles by item type can be found \href{https://docs.planet.com/develop/apis/orders/product_bundles/}{here}.
#' @details The `cloud_lim` parameter is set to 1 by default, which means we will download all images regardless of cloud coverage. You can set this parameter to a lower value (e.g., 0.5) to filter out images with more than 50% cloud coverage.
#' @details PlanetScope API allow users to apply a tool named "harmonize" that applies scene-level normalization and harmonization, such that all PlanetScope data were consistent and approximately comparable to data from Sentinel-2. This tool is useful when integrating or comparing images from different times and locations. Refer to \href{https://assets.planet.com/docs/scene_level_normalization_of_planet_dove_imagery.pdf}{PlanetScope technical documentation} for more details.
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
    stop("Invalid API key. Please provide a valid API key. You may sign up for an account and get an API key at {.url https://www.planet.com/get-started/}.")
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
#' @note You will need an active Planet account and an API key to access the PlanetScope API. You can sign up for an account on the \href{https://www.planet.com/get-started/}{Planet website}. Once you have an account, you can copy your API key from your account settings.
#'
#' @param change_key Logical. If `TRUE`, prompts for a new API key even if one already exists (default: `FALSE`).
#'
#' @return Invisibly returns the API key.
#'
#' @examples
#' \dontrun{
#' set_api_key() # Set the API key for the first time
#' set_api_key(change_key = TRUE) # Change the API key
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
    write(stringr::str_c("planet_api_key", "=", key), file = ".env", append = TRUE) # Save to .env
  } else {
    if (change_key) {
      # If the user opts to change the key, ask for a new key
      new_key <- readline("Enter new Planet API key: ")
      # Replace or add the new API key in the .env file
      key_lines <- readLines(".env")
      key_lines <- gsub("^planet_api_key=.*", stringr::str_c("planet_api_key=", new_key), key_lines)
      writeLines(key_lines, ".env")
      key <- new_key # Update the key variable
    }
  }

  invisible(key)
}
