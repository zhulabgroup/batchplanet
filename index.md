# BatchPlanet

**BatchPlanet** is an R package designed to automate workflows for
downloading, processing, and visualizing remote sensing imagery in batch
from the Planet API. Originally developed to study tree phenology [Song
et al., (2025)](https://doi.org/10.1016/j.srs.2025.100205), the package
has been generalized for broader environmental applications. It provides
a suite of tools to:

- **Download Data:** Retrieve PlanetScope images across multiple sites
  over time.
- **Process Data:** Extract reflectance time series, calculate
  vegetation indices (e.g., Enhanced Vegetation Index), and estimate
  start/end of season.
- **Visualize Data:** Generate interactive true color maps and time
  series plots.

------------------------------------------------------------------------

## Features

- **Batch Data Retrieval:** Download data over a large area (e.g.,
  several cities) and a long time window (e.g., 2017 to current).
- **High-Performance Capabilities:** Optimized for HPC environments with
  parallel processing support.
- **Interactive Visualizations:** Create interactive true-color imagery
  gallery and time series plots.
- **Robust Processing Tools:** Provide generalizable nonparameteric time
  series analysis tools (e.g., weighted Whittaker smoothing,
  threshold-based start/end of season metrics estimation).

------------------------------------------------------------------------

## Installation

Install the stable version from R-Universe:

``` r
install.packages('BatchPlanet', repos = c('https://yiluansong.r-universe.dev', 'https://cloud.r-project.org'))
```

Install the development version from GitHub:

``` r
# Install devtools if not already installed
install.packages("devtools")
devtools::install_github("zhulabgroup/BatchPlanet")
```

This package was developed and tested using the
[`rocker/geospatial:4.2.0-2023-04-25`](https://hub.docker.com/r/rocker/geospatial)
Docker image, with RStudio 2024.04.01 and GCC 10.3.0.

------------------------------------------------------------------------

## Examples and documentation

For complete guides on using all features of `BatchPlanet`, please
explore our package vignettes:

- [**Getting started
  workflow**](https://zhulabgroup.github.io/BatchPlanet/articles/1_workflow.html):
  A complete, step-by-step example demonstrating how to download
  PlanetScope data and process time series.
- [**Customization and advanced
  usage**](https://zhulabgroup.github.io/BatchPlanet/articles/2_customization.html):
  Advanced configurations for tailoring the processing pipeline to
  specific research needs.
- [**Miscellaneous time series processing
  tools**](https://zhulabgroup.github.io/BatchPlanet/articles/3_tools.html):
  Two generalizable tools for time series analysis.

Full documentation of functions can be found on the [Reference
page](https://zhulabgroup.github.io/BatchPlanet/reference/index.html).

------------------------------------------------------------------------

## Testing

We have developed **testthat** unit tests covering all core functions.

``` r
# from within R or RStudio, with your working directory set to the package root
devtools::test()
```

[![codecov](https://codecov.io/gh/zhulabgroup/BatchPlanet/graph/badge.svg?token=YOUR_TOKEN)](https://codecov.io/gh/zhulabgroup/BatchPlanet)

------------------------------------------------------------------------

## Citation

To cite this package in publications use:

Song Y, Bevington A, Reid K, Zhu J, Liu Y, Zhu K (2025). *BatchPlanet:
Batch access and processing of PlanetScope imagery for spatiotemporal
analysis in R*. R package version 0.1.0,
<https://github.com/zhulabgroup/BatchPlanet>.

A BibTeX entry for LaTeX users is

``` bibtex
  @Manual{,
    title = {BatchPlanet: Batch access and processing of PlanetScope imagery for spatiotemporal analysis in R},
    author = {Yiluan Song and Alexandre R. Bevington and Ken Reid and Jiali Zhu and Yi Liu and Kai Zhu},
    year = {2025},
    note = {R package version 0.1.0},
    url = {https://github.com/zhulabgroup/BatchPlanet},
  }
```

------------------------------------------------------------------------

## Community Guidelines

We welcome contributions from the community to help improve
`BatchPlanet`.

If you would like to contribute code, fix a bug, or suggest a new
feature, please:

1.  **Fork** the repository.
2.  Create a new **branch** for your changes. We recommend following the
    [Tidyverse Style Guide](https://style.tidyverse.org/) for R code.
3.  Submit a **Pull Request** (PR) detailing your changes.

If you encounter a bug or have a suggestion for improvement, please open
an issue in the [GitHub Issue
Tracker](https://github.com/zhulabgroup/BatchPlanet/issues). Please
provide a minimal reproducible example (reprex) if reporting a bug.

If you have questions regarding the usage of the package or need help
with a specific workflow, please contact the maintainer, Yiluan Song, at
<songyl@umich.edu>.

------------------------------------------------------------------------

## Planned Features (Coming Soon)

The following features are planned for future releases of BatchPlanet:

- **Polygon support:** Ability to use polygons (or multiple polygons) as
  the area of interest (AOI) for ordering, downloading, and extracting
  data, in addition to points.
- **Band math and raster processing tools:** Integration with the Planet
  Orders API’s processing tools (such as band math, clip, harmonize,
  reproject, tile, and more) to allow on-the-fly calculation of indices
  (e.g., NDVI, EVI) and other raster operations before download. See
  [Planet Orders API Tools
  documentation](https://docs.planet.com/develop/apis/orders/tools/).
- **Option to run in series:** Ability to choose between parallel and
  serial (series) processing for workflows where parallelization is not
  desired or possible.

Have a feature request? Please open an issue or discussion!

------------------------------------------------------------------------

Enjoy exploring and analyzing remote sensing data with BatchPlanet! 🚀
