# Place a PlanetScope order

Submits an order to Planet API, with specified image IDs and tools.

## Usage

``` r
order_planetscope_imagery(
  api_key,
  bbox,
  items,
  item_name,
  product_bundle,
  harmonized,
  order_name,
  mostrecent = 0
)
```

## Arguments

- api_key:

  Character. Your Planet API key.

- bbox:

  An `sf`-style bounding box (a named numeric list with
  `xmin,ymin,xmax,ymax`).

- items:

  Character vector. Planet image IDs to include in this order.

- item_name:

  Character. Item type (e.g. `"PSScene"`).

- product_bundle:

  Character. Product bundle (e.g. `"analytic_sr_udm2"`).

- harmonized:

  Logical. If `TRUE`, applies harmonization tool in the order.

- order_name:

  Character. A unique name for this order.

- mostrecent:

  Integer. If \>0, only the most recent N images are ordered (default: 0
  = all).

## Value

Character. The ID of the order.

## Examples

``` r
if (FALSE) { # \dontrun{
order_id <- order_planetscope_imagery(
  api_key = set_api_key(),
  bbox = set_bbox(df_coordinates, "SJER"),
  items = ids,
  item_name = "PSScene",
  product_bundle = "analytic_sr_udm2",
  harmonized = TRUE,
  order_name = "SJER_20240501_20240531",
  mostrecent = 0
)
} # }
```
