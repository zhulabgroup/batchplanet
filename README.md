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

***

## Testing

We have basic **testthat** unit tests covering our core, pure-R functionality:

- **whittaker_smoothing_filling()**  
  Checks that a perfect linear ramp remains highly correlated after smoothing, and that gaps longer than `maxgap` stay as `NA`.

- **determine_seasonality()**  
  Verifies that a flat sequence is correctly flagged non-seasonal, and a sinusoidal sequence is flagged seasonal (with warnings suppressed).

- **read_data_product()**  
  Confirms that `.rds` files in a product folder are read into a single data frame and that `site`/`group` are extracted from the filename.

To run all tests from your package root in R:

```r
# from within R or RStudio, with your working directory set to the package root
devtools::test()
```

## Example Workflow

For a complete example of the package in action, see **vignettes/vignette.Rmd**. This document demonstrates steps in downloading PlanetScope data and processing time series using a subset of NEON site data.

***

Enjoy exploring and analyzing remote sensing data with BatchPlanet! 🚀

***
