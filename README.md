# BatchPlanet 🌿📊

**BatchPlanet** is an R package designed to automate workflows for downloading, processing, and visualizing remote sensing imagery in batch from the Planet API. Originally developed to study tree phenology, the package has been generalized for broader environmental applications. It provides a suite of tools to:

* **Download Data:** Retrieve PlanetScope images across multiple sites over time.
* **Process Data:** Extract reflectance time series, calculate remote sensing indices (e.g., Enhanced Vegetation Index), and estimate transition time (e.g., green-up date).
* **Visualize Data:** Generate visually appealing plots for raster maps and time series.

***

## Features

* **Flexible Data Retrieval:** Download data over a large area (e.g., several cities) and a long time window (e.g., 2017 to current).
* **High-Performance Capabilities:** Optimized for HPC environments with parallel processing support.
* **Informative Visualizations:** Create true-color imagery and time series plots.
* **Robust Processing Tools:** Provide generalizable time series analysis tools (e.g., weighted Whittaker smoothing, transition time estimation).
* **Extensive Testing:** Comprehensive test suite with interactive prompts for API key input and download test decisions.


***

## Installation

Install the development version from GitHub:

```r
# Install devtools if not already installed
install.packages("devtools")
devtools::install_github("zhulabgroup/phenology-batchplanet")
```

***

## Usage

### 1. Setting Up the Environment

Before processing data, set up the test environment (which creates necessary directories and sample data):

```r
source("tests/setup_test.R")
```

### 2. Downloading Data

Use the master test runner to securely input your Planet API key and decide whether to run download tests:

```r
source("tests/run_tests.R")
```

Follow the prompts to enter your API key and to choose if you want to execute download tests.

### 3. Processing Data

For example, to process satellite time series data:

```r
result <- process_satellite_ts(
  dir = "test_data",
  df_coords = readRDS("test_data/df_coords.rds"),
  v_site = "HARV",
  v_group = "TestGroup"
)
```

Or to process DOY data:

```r
proc_doy(dir = "alldata/PSdata/")
```

### 4. Visualizing Data

Generate visualizations with functions like:

```r
plot_ts <- visualize_ts(df_ts, n_id = 2, smooth = TRUE, save_plot = TRUE, plot_path = "plots")
plot_raster <- visualize_raster("test_data/sample_raster.tif", save_plot = TRUE, plot_path = "plots")
```

### 5. Example Workflow

For a complete example of the package in action, see the **workflow\_neon\_subset.Rmd** vignette. This document demonstrates an analysis workflow using a subset of NEON site data (focusing on HARV and SJER):

```r
df_coords <- read_csv("data/neon_subset/metadata.csv", show_col_types = FALSE)
df_coords <- df_coords %>% filter(site %in% c("HARV", "SJER"))
```

This workflow covers reading coordinate data, setting bounding boxes, processing time series, calculating DOY metrics, and visualizing the results.

***

## Future Directions

* **Generalization:** Enable users to calculate custom indices and process non-vegetation data by specifying custom grouping variables.
* **Cloud Integration:** Explore integration with cloud platforms like Google Earth Engine for scalable processing.
* **Enhanced Visualization:** Refine plotting functions for more informative and accessible outputs (e.g., additional color-blind friendly palettes).
* **HPC Optimization:** Provide guidelines for running the package efficiently in high-performance computing environments.

***

## Acknowledgments

Developed by **Yiluan Song** (lead author) with contributions from Ken Reid and collaborators. 🙌

***

Enjoy exploring and analyzing remote sensing data with BatchPlanet! 🚀

***
