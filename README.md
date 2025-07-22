# BatchPlanet 🌿📊

**BatchPlanet** is an R package designed to automate workflows for downloading, processing, and visualizing remote sensing imagery in batch from the Planet API. Originally developed to study tree phenology, the package has been generalized for broader environmental applications. It provides a suite of tools to:

* **Download Data:** Retrieve PlanetScope images across multiple sites over time.
* **Process Data:** Extract reflectance time series, calculate remote sensing indices (e.g., Enhanced Vegetation Index), and estimate transition time (e.g., green-up date).
* **Visualize Data:** Generate interactive true color maps and time series plots.

***

## Features

* **Batch Data Retrieval:** Download data over a large area (e.g., several cities) and a long time window (e.g., 2017 to current).
* **High-Performance Capabilities:** Optimized for HPC environments with parallel processing support.
* **Interactive Visualizations:** Create interactive true-color imagery gallery and time series plots.
* **Robust Processing Tools:** Provide generalizable nonparameteric time series analysis tools (e.g., weighted Whittaker smoothing, threshold-based phenological metrics estimation).

***

## Installation

Install the development version from GitHub:

```r
# Install devtools if not already installed
install.packages("devtools")
devtools::install_github("zhulabgroup/phenology-batchplanet")
```

This package was developed and tested using the [`rocker/geospatial:4.2.0-2023-04-25`](https://hub.docker.com/r/rocker/geospatial) Docker image, with RStudio 2024.04.01 and GCC 10.3.0.

***

## Example Workflow

For a complete example of the package in action, see **vignettes/vignette.Rmd**. This document demonstrates steps in downloading PlanetScope data and processing time series using a subset of NEON site data.

***

## Testing

We have developed **testthat** unit tests covering all core functions.

```r
# from within R or RStudio, with your working directory set to the package root
devtools::test()
```

***

## Planned Features (Coming Soon)

The following features are planned for future releases of batchplanet:

- **Polygon support:** Ability to use polygons (or multiple polygons) as the area of interest (AOI) for ordering, downloading, and extracting data, in addition to points.
- **Band math and raster processing tools:** Integration with the Planet Orders API's processing tools (such as band math, clip, harmonize, reproject, tile, and more) to allow on-the-fly calculation of indices (e.g., NDVI, EVI) and other raster operations before download. See [Planet Orders API Tools documentation](https://docs.planet.com/develop/apis/orders/tools/).
- **Option to run in series:** Ability to choose between parallel and serial (series) processing for workflows where parallelization is not desired or possible.

Have a feature request? Please open an issue or discussion!

***

Enjoy exploring and analyzing remote sensing data with BatchPlanet! 🚀
