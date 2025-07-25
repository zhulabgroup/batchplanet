% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_planetscope_parameters.R
\name{set_data_directory}
\alias{set_data_directory}
\title{Set or change the data directory}
\usage{
set_data_directory(change_directory = F)
}
\arguments{
\item{change_directory}{Logical. If \code{TRUE}, prompts for a new directory even if one already exists (default: \code{FALSE}).}
}
\value{
Character. The path to the data directory.
}
\description{
Prompts the user to enter a path to the data directory and saves it in a hidden \code{.env} file in the working directory.
}
\examples{
\dontrun{
set_data_directory() # Set the data directory for the first time
set_data_directory(change_directory = TRUE) # Change the data directory
}

}
