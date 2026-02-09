# Set PlanetScope API Parameters

Constructs a named list of parameters required for interacting with the
PlanetScope API.

## Usage

``` r
set_planetscope_parameters(
  api_key,
  item_name = "PSScene",
  asset = "ortho_analytic_4b_sr",
  product_bundle = "analytic_sr_udm2",
  cloud_lim = 1,
  harmonized = TRUE
)
```

## Arguments

- api_key:

  Character. API key for authentication. We recommend using
  [`set_api_key()`](https://zhulabgroup.github.io/BatchPlanet/reference/set_api_key.md)
  to save your API key in a hidden .env file.

- item_name:

  Character. Name of the satellite data product (default: `"PSScene"`).

- asset:

  Character. Type of asset to retrieve (default:
  `"ortho_analytic_4b_sr"`).

- product_bundle:

  Character. Product bundle selection (default: `"analytic_sr_udm2"`).

- cloud_lim:

  Numeric. Cloud coverage limit (between 0 and 1). Images with a cloud
  coverage above this fraction will be ignored. (default: 1).

- harmonized:

  Logical. Indicates whether to use the Planset API tool to harmonize
  data with Sentinel-2 (default: `TRUE`).

## Value

A named list containing the PlanetScope API parameters.

## Details

You will need an active Planet account and an API key to access the
PlanetScope API. You can sign up for an account on the [Planet
website](https://www.planet.com/get-started/). Once you have an account,
you can copy your API key from your account settings.

An **item_type** represents an imagery product. The default item type is
"PSScene", which is PlanetScope 3, 4, and 8 band scenes captured by the
Dove satellite constellation. Other item types include "TanagerScene",
"TanagerMethane", "REOrthoTile", "REScene", "SkySatScene",
"SkySatCollect", and "SkySatVideo". Please refer to [the Planet
catalog](https://docs.planet.com/data/imagery/) for the most updated
available item types.

An **asset** is a product derived from the item's source data, and can
be used for various analytic, visual or other purposes. For example, a
full list of available assets for "PSScene" can be found
[here](https://docs.planet.com/data/imagery/planetscope/psscene/).
Please visit [the Planet catalog](https://docs.planet.com/data/imagery/)
for a full list of available assets for each item type.

A **product bundle** comprises of a group of assets for an item. This is
often useful when you want to download associated metadata and data
quality masks. A full list of bundles by item type can be found
[here](https://docs.planet.com/develop/apis/orders/product_bundles/).

The `cloud_lim` parameter is set to 1 by default, which means we will
download all images regardless of cloud coverage. You can set this
parameter to a lower value (e.g., 0.5) to filter out images with more
than 50% cloud coverage.

PlanetScope API allow users to apply a tool named "harmonize" that
applies scene-level normalization and harmonization, such that all
PlanetScope data were consistent and approximately comparable to data
from Sentinel-2. This tool is useful when integrating or comparing
images from different times and locations. Refer to [PlanetScope
technical
documentation](https://assets.planet.com/docs/scene_level_normalization_of_planet_dove_imagery.pdf)
for more details.

## Examples

``` r
if (FALSE) { # \dontrun{
setting <- set_planetscope_parameters(
  api_key = set_api_key(),
  item_name = "PSScene",
  asset = "ortho_analytic_4b_sr",
  product_bundle = "analytic_sr_udm2",
  cloud_lim = 1,
  harmonized = TRUE
)
} # }
```
