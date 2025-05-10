# BatchPlanet 🌿📊

**BatchPlanet** is an R package designed to automate workflows for downloading, processing, and visualizing remote sensing imagery in batch from the Planet API. Originally developed to study tree phenology, the package has been generalized for broader environmental applications. It provides a suite of tools to:

* **Download Data:** Retrieve PlanetScope images across multiple sites over time.
* **Process Data:** Extract reflectance time series, calculate remote sensing indices (e.g., Enhanced Vegetation Index), and estimate transition time (e.g., green-up date).
* **Visualize Data:** Generate visually appealing plots for raster maps and time series.

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

## Example Workflow

For a complete example of the package in action, see **vignettes/vignette.Rmd**. This document demonstrates steps in downloading PlanetScope data and processing time series using a subset of NEON site data.

***

## Future Directions

* **Generalization:** Enable users to calculate custom indices and process non-vegetation data by specifying custom grouping variables.
* **Cloud Integration:** Explore integration with cloud platforms like Google Earth Engine for scalable processing.
* **HPC Optimization:** Provide guidelines for running the package efficiently in high-performance computing environments.

***

## Acknowledgments

Developed by **Yiluan Song** (lead author) with contributions from Ken Reid and collaborators. 🙌

***

Enjoy exploring and analyzing remote sensing data with BatchPlanet! 🚀

***
