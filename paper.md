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
  - name: Kai Zhu
    orcid: 0000-0003-1587-3317
    affiliation: 2
affiliations:
  - name: Michigan Institute for Data and AI in Society, University of Michigan, Ann Arbor, MI, USA
    index: 1
  - name: School for Environment and Sustainability and Institute for Global Change Biology, University of Michigan, Ann Arbor, MI, USA
    index: 2
date: 2025-05-11
repository: https://github.com/zhulabgroup/batchplanet
---

# Summary

The `batchplanet` R package provides a reproducible and scalable workflow for accessing and processing PlanetScope satellite imagery, enabling environmental researchers to perform spatiotemporal analysis of high-resolution remote sensing data. The package streamlines the steps required to work with the PlanetScope API, including the ordering and downloading of imagery, retrieving and cleaning pixel-level time series, and computing derived metrics such as the Enhanced Vegetation Index (EVI) and green-up/down time. This tool has supported peer-reviewed research in predicting reproductive phenology in wind-pollinated trees [@song2025phenology]. Generalizable beyond phenological research, this tool is particularly suited for environmental research that involves large volumes of imagery across multiple sites and long time periods.

# Statement of Need

PlanetScope is a high-resolution (~3-meter), near-daily satellite imagery product provided by Planet Labs. With its global coverage, high spatial resolution, and rapid revisit time, PlanetScope data has become increasingly valuable for scientific research and operational monitoring — enabling detailed analyses of land use change, ecosystem dynamics, disaster impacts, and more [@moon2021phenology]. While access to Planet data has been facilitated through the Planet API [@planet2017api], using such data still poses challenges: complex API interactions, large-volume data downloads, and non-trivial processing workflows often limit accessibility and reproducibility.

Cloud-based platforms such as Sentinel Hub and Google Earth Engine (GEE) provide powerful PlanetScope imagery access and processing capabilities, but their cloud-based nature restricts user control over processing environments and data storage, which can complicate reproducibility and transparency in scientific workflows. In addition, the programming languages (Python and JavaScript) used on these two platforms might be less familiar to R users. An existing R package `planetR` offers an R-native interface to the Planet API. Nevertheless, these existing tools usually require users to write custom scripts to download and process data in batch over multiple sites, which can be time-consuming and is easily limited by users' computing and storage resources. `batchplanet` fills key gaps in the existing ecosystem (Table 1) with R-native, locally reproducible batch downloading and processing of PlanetScope imagery. It also includes features such as time series analysis tools and interactive visualization. `batchplanet` is particularly suitable for scientific research workflows that require scalability, transparency, and reproducibility over the data pipeline.


| Feature / Tool                        | **batchplanet**                  | **Sentinel Hub**                 | **Google Earth Engine (GEE)**        | **planetR (Bevington)**           |
|---------------------------------------|----------------------------------|----------------------------------|--------------------------------------|-----------------------------------|
| **Primary Language**                  | R                                | Python                           | JavaScript / Python                  | R                                 |
| **Processing Environment**            | Local                            | Cloud                            | Cloud                                | Local                             |
| **Data Control & Reproducibility**    | High                             | Moderate                         | Low                                  | High                              |
| **Batch Processing**                  | Streamlined                      | Supported for enterprise users   | Via scripting                        | Via scripting                     |
| **Time Series Analysis Tools**        | Supported                        | Limited                          | Supported                            | Not supported                     |
| **Interactive Visualization**         | Supported                        | Limited                          | Supported                            | Not supported                     |
: Table 1. Comparison of `batchplanet` with existing tools for PlanetScope data access and processing, including Sentinel Hub, Google Earth Engine (GEE), and the `planetR` package by Bevington.

The package is particularly beneficial to researchers and practitioners who:
- Conduct time series analyses across spatially dispersed monitoring sites.
- Prioritize reproducibility in remote sensing workflows.
- Work mainly in R and seek alternatives to Python-based tools.
- Use high-performance computing (HPC) infrastructure.

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
