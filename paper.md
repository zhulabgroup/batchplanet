---
title: "batchplanet: Batch access and processing of PlanetScope imagery for spatiotemporal analysis in R"
tags:
  - R
  - remote sensing
  - PlanetScope
  - phenology
  - imagery processing
  - time series
authors:
  - name: Yiluan Song
    orcid: 0000-0003-3660-3797
    affiliation: 1,2
  - name: Ken Reid
    orcid: 0000-0001-8654-2430
    affiliation: 1
  - name: Yi Liu
    orcid: 0000-0002-5515-8804
    affiliation: 2
  - name: Jiali Zhu
    orcid: 0009-0009-7339-557X
    affiliation: 2
  - name: Kai Zhu
    orcid: 0000-0003-1587-3317
    affiliation: 2
  - name: Alexandre R. Bevington
    orcid: 0000-0003-1587-3317
    affiliation: 3 4
affiliations:
  - name: Michigan Institute for Data and AI in Society, University of Michigan, Ann Arbor, MI, USA
    index: 1
  - name: School for Environment and Sustainability and Institute for Global Change Biology, University of Michigan, Ann Arbor, MI, USA
    index: 2
  - name: Department of Geography, Earth and Environmental Sciences, University of Northern British Columbia, Prince George, BC, Canada
    index: 3
  - name: Ministry of Forests, Province of British Columbia, Prince George, BC, Canada
    index: 4
date: 2025-05-11
repository: https://github.com/zhulabgroup/batchplanet
---

# Summary

The `batchplanet` R package provides a reproducible and scalable workflow for accessing and processing PlanetScope satellite imagery, enabling environmental researchers to efficiently perform spatiotemporal analysis of high-resolution remote sensing data. The package streamlines the steps required to work with the PlanetScope API, including the ordering and downloading of imagery, retrieving and cleaning pixel-level time series, and computing derived metrics such as the Enhanced Vegetation Index (EVI) and green-up/down time. This package has supported peer-reviewed research in predicting reproductive phenology in wind-pollinated trees [@song2025phenology]. Generalizable beyond phenological research, this tool is particularly suited for environmental research that involves large volumes of imagery across multiple sites and long time periods.

# Statement of Need

PlanetScope is a commercial high-resolution (~3-meter), near-daily satellite imagery product provided by Planet Labs. With its global coverage, high spatial resolution, and rapid revisit time, PlanetScope data has become increasingly valuable for scientific research and operational monitoring — enabling detailed analyses of land use change, ecosystem dynamics, disaster impacts, and more [@moon2021phenology]. While access to Planet data has been facilitated through the Planet API [@planet2017api], using such data still poses challenges: complex API interactions, large-volume data downloads, and non-trivial processing workflows often limit accessibility and reproducibility.

Planet Labs provides an official [Python SDK](https://planet-sdk-for-python-v2.readthedocs.io/en/latest/python/sdk-guide/) for programmatic access to the Planet APIs, supporting both Python scripting and a no-code CLI. Cloud-based platforms such as Sentinel Hub and Google Earth Engine (GEE) provide powerful PlanetScope imagery access and processing capabilities, but their cloud-based nature restricts user control over processing environments and data storage, which can complicate reproducibility and transparency in scientific workflows. The programming languages (Python and JavaScript) used on these three platforms might be less familiar to R users. An existing R package `planetR` offers an R-native interface to the Planet API. Nevertheless, these existing tools usually require users to write custom scripts to download and process data in batch over multiple sites, which can be time-consuming and is easily limited by users' computing and storage resources. `batchplanet` fills key gaps in the existing ecosystem (Table 1) with R-native, locally reproducible batch downloading and processing of PlanetScope imagery. It also includes features such as time series analysis tools and interactive visualization. `batchplanet` is particularly suitable for scientific research workflows that require scalability, transparency, and reproducibility over the data pipeline.

| Feature / Tool                        | **Planet Python SDK**            | **Sentinel Hub**                 | **Google Earth Engine (GEE)**        | **planetR (Bevington)**           | **batchplanet**                   |
|---------------------------------------|----------------------------------|----------------------------------|--------------------------------------|-----------------------------------|-----------------------------------|
| **Primary Language**                  | Python                           | Python                           | JavaScript / Python                  | R                                 | R                                 |
| **Processing Environment**            | Local/Cloud                      | Cloud                            | Cloud                                | Local                             | Local                             |
| **Data Control & Reproducibility**    | High                             | Moderate                         | Low                                  | High                              | High                              |
| **Batch Processing**                  | Via scripting/CLI                | Supported for enterprise users   | Via scripting                        | Via scripting                     | Streamlined                       |
| **Time Series Analysis Tools**        | Not supported                    | Limited                          | Supported                            | Not supported                     | Supported                         |
| **Interactive Visualization**         | Not supported                    | Limited                          | Supported                            | Not supported                     | Supported                         |

: Table 1. Comparison of `batchplanet` with existing tools for PlanetScope data access and processing, including the official Planet Python SDK, Sentinel Hub, Google Earth Engine (GEE), the `planetR` package by Bevington.

# Key Features

The package is particularly beneficial to researchers and practitioners who:
- Conduct time series analyses across spatially dispersed monitoring sites.
- Work mainly in R and seek alternatives to Python-based tools.
- Prioritize reproducibility in remote sensing workflows.
- Use high-performance computing (HPC) infrastructure.
- Visualize PlanetScope imagery, and processed data products, interactively.

A challenge for researchers using PlanetScope imagery is the need to download large volumes of data over a long timespan. This is particularly important for those working on phenology (the seasonality of the biological systems), as this type of research requires reflectances on high frequency to capture critical events, as well as multiple years to monitor interannual changes. Users often hit Planet API rate limit when trying to download thousands of images at once. The `batchplanet` package addresses this challenge by providing a streamlined workflow for batch ordering and downloading, by searching for images by month and splitting large amount of available images into smaller orders. This ensures that users will get complete data for their sites of interest without hitting the API rate limit. Another challenge is the need to download images from multiple sites that are spatially distant. If attempting to use one area of interest that covers all sites, users may end up downloading many images that are not relevant to their sites of interest. The `batchplanet` package allows users to specify multiple sites, each with a set of coordinates of interest, and can parallelize the downloading process across these sites.

`batchplanet` provides a series of functions to facilitate the entire R-native workflow of accessing and processing PlanetScope imagery, especially for temporal analysis. These include functions to order and download PlanetScope imagery, retrieve pixel-level time series data, clean reflectance time series, calculate the Enhanced Vegetation Index (EVI), and compute phenological metrics such as green-up and green-down dates. Apart from the streamlined batch processing functions, `batchplanet` provides individual functions for key steps of the workflow, allowing users to customize their data processing pipelines. `batchplanet` also provides functions to visualize true color images interactively, enabling users to explore spatiotemporal patterns in the data (Fig. 1).

With this package, we significantly speed up the ordering, downloading, and processing of PlanetScope images. For example, images for an approximately $9 km^2$ area in one month was downloaded in $6.7$ seconds. As we allowed parallel downloading across months, the total time for downloading a year of PlanetScope images was similar. From on month of downloaded images at one site, we retrieved time series of reflectances, together with quality mask data and metadata, at 100 coordinates of interest in $20.9$ seconds. This retrieval could again be parallelized over multiple sites and groups of coordinates.


**Figure 1.** Screenshot of the true color image viewer for PlanetScope imagery. The viewer allows users to visualize true color images at different site and time interactively, facilitating the exploration of spatiotemporal patterns.

# Installation

```r
# Install from GitHub using remotes
remotes::install_github("zhulabgroup/batchplanet")
```

# Example Usage
```r
library(batchplanet)

# Read example coordinates
df_coordinates <- read_csv(system.file("extdata", "example_neon_coordinates.csv", package = "batchplanet"))
visualize_coordinates(df_coordinates)
```

```r
# Set download parameters and data directory
setting <- set_planetscope_parameters(
  api_key = set_api_key(),
  item_name = "PSScene",
  asset = "ortho_analytic_4b_sr",
  product_bundle = "analytic_sr_udm2",
  cloud_lim = 0.3,
  harmonized = TRUE
)
dir_data <- set_data_directory()
```

```e
# Order and download imagery (slow process, uncomment to run)
# order_planetscope_imagery_batch(dir = dir_data, df_coordinates = df_coordinates, v_site = c("HARV", "SJER"), v_year = 2024, setting = setting)
# download_planetscope_imagery_batch(dir = dir_data, setting = setting, num_cores = 3)
visualize_true_color_imagery_batch(dir = dir_data, df_coordinates = df_coordinates)
```

```r
# Retrieve time series
# retrieve_planetscope_time_series_batch(dir = dir_data, df_coordinates = df_coordinates, num_cores = 10)
df_ts <- read_data_product(dir = dir_data, product_type = "ts")
visualize_time_series(df_ts, var = "green", ylab = "Green reflectance", facet_var = "site", smooth = F)
```

```r
# Clean time series and calculate EVI
# clean_planetscope_time_series_batch(dir = dir_data, num_cores = 3, calculate_evi = T)
df_clean <- read_data_product(dir = dir_data, product_type = "clean")
visualize_time_series(df_clean, var = "evi", ylab = "EVI", facet_var = "site", smooth = T)
```

```r
# Calculate phenological metrics
df_thres <- set_thresholds(thres_up = c(0.3, 0.4, 0.5), thres_down = NULL)
# calculate_phenological_metrics_batch(dir = dir_data, v_site = "SJER", v_group = "Quercus", df_thres = df_thres, var_index = "evi", num_cores = 3)
v_id <- c("NEON.PLA.D17.SJER.06001", "NEON.PLA.D17.SJER.06337", "NEON.PLA.D17.SJER.06310")
df_doy_sample <- read_data_product(dir = dir_data, product_type = "doy") %>% filter(id %in% v_id)
df_evi_sample <- read_data_product(dir = dir_data, product_type = "clean") %>% filter(id %in% v_id)
visualize_time_series(df_ts = df_evi_sample, df_doy = df_doy_sample, var = "evi", ylab = "EVI", facet_var = "id", smooth = T)
```

# Acknowledgements
Yiluan Song was supported by the Eric and Wendy Schmidt AI in Science Postdoctoral Fellowship, a Schmidt Sciences program. Kai Zhu and Yiluan Song were supported by the National Science Foundation [grant numbers 2306198 (CAREER)]. We thank the Planet team for providing access to their API.

# References
@article{song2025phenology,
  title={Predicting reproductive phenology of wind-pollinated trees via PlanetScope time series},
  author={Song, Y. and Katz, D. S. and Zhu, Z. and Beaulieu, C. and Zhu, K.},
  journal={Science of Remote Sensing},
  year={2025},
  pages={100205},
  doi={10.1016/j.srs.2025.100205}
}
@article{moon2021phenology,
  title={Multiscale assessment of land surface phenology from harmonized Landsat 8 and Sentinel-2, PlanetScope, and PhenoCam imagery},
  author={Moon, M. and Richardson, A. D. and Friedl, M. A.},
  journal={Remote Sensing of Environment},
  volume={266},
  pages={112716},
  year={2021},
  doi={10.1016/j.rse.2021.112716}
}
@misc{planet2017api,
  author = {Planet Team},
  title = {Planet application program interface: In space for life on Earth},
  year = {2017},
  howpublished = {\url{https://api.planet.com}}
}
