#' Download sample data from GitHub
#'
#' This function clones the `sample-data` branch of the
#' [BatchPlanet GitHub repository](https://github.com/zhulabgroup/BatchPlanet)
#' into the current working directory.
#'
#' @details
#' Requires that `git` is installed and available on your system PATH.
#' If the `sample-data` folder already exists, the function will not overwrite it.
#'
#' @return
#' Path where the data was downloaded to.
#' @export
download_sample_data <- function() {
  # Target repo and branch
  repo_url <- "https://github.com/zhulabgroup/BatchPlanet.git"
  branch <- "sample-data"
  dest_dir <- file.path(getwd(), "sample-data")

  # Check for git
  if (Sys.which("git") == "") {
    stop("Git is not installed or not found in your system PATH.")
  }

  # Avoid overwriting
  if (dir.exists(dest_dir)) {
    message("Directory 'sample-data' already exists. Skipping download.")
    return(invisible(dest_dir))
  }

  # Clone the branch
  cmd <- sprintf(
    "git clone --branch %s --single-branch %s %s",
    branch, repo_url, shQuote(dest_dir)
  )

  status <- system(cmd)

  if (status != 0) {
    stop("Git clone failed. Please check your internet connection or repository URL.")
  } else {
    message("Sample data successfully downloaded to ", dest_dir)
  }

  invisible(dest_dir)
}
