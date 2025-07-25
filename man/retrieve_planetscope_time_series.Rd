% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/retrieve_planetscope_time_series.R
\name{retrieve_planetscope_time_series}
\alias{retrieve_planetscope_time_series}
\title{Retrieve a single set of PlanetScope time series}
\usage{
retrieve_planetscope_time_series(dir_site, sf_coordinates, num_cores = 12)
}
\arguments{
\item{dir_site}{Character. Path to the site-specific raw data directory.}

\item{sf_coordinates}{An \code{sf} object with point coordinates (must have an \code{id} column).}

\item{num_cores}{Integer. Number of parallel workers for processing (default: 12).}
}
\value{
Data frame with columns for point ID, coordinates, reflectance bands, QA, and metadata.
}
\description{
Extracts and combines reflectance, QA, and metadata for a set of spatial points.
}
\examples{
\dontrun{
df_coordinates_example <- df_coordinates \%>\% filter(site == "SJER", group == "Quercus")
df_ts_example <- retrieve_planetscope_time_series(
  dir_site = file.path("alldata/PSdata/raw", "SJER"),
  sf_coordinates = sf::st_as_sf(df_coordinates_example, coords = c("lon", "lat"), crs = 4326),
  num_cores = 12
)
}

}
