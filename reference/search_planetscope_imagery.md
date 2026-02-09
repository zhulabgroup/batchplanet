# Search for available PlanetScope imagery

Queries the Planet API for imagery overlapping the specified bounding
box and date range, then filters results by cloud cover, ground control,
and quality. Returns only IDs for which you have download permission.

## Usage

``` r
search_planetscope_imagery(
  api_key,
  bbox,
  date_end = NULL,
  date_start = NULL,
  item_name = "PSScene",
  asset = "ortho_analytic_4b_sr",
  cloud_lim = 0.1,
  ground_control = TRUE,
  quality = "standard"
)
```

## Arguments

- api_key:

  Character. Your Planet API key.

- bbox:

  Named numeric list with `xmin`, `ymin`, `xmax`, `ymax` defining the
  search area.

- date_end:

  Character. End date ("YYYY-MM-DD") for the search (inclusive).

- date_start:

  Character. Start date ("YYYY-MM-DD") for the search (inclusive).

- item_name:

  Character. Planet item type to search (default: `"PSScene"`).

- asset:

  Character. Asset type to filter permissions (default:
  `"ortho_analytic_4b_sr"`).

- cloud_lim:

  Numeric. Maximum allowed cloud cover fraction (0–1, default: 0.1).

- ground_control:

  Logical. If `TRUE`, require ground control metadata (default: `TRUE`).

- quality:

  Character. Quality category filter (default: "standard").

## Value

Character vector of image IDs with download permission. Returns `NULL`
if none are found.

## Examples

``` r
if (FALSE) { # \dontrun{
ids <- search_planetscope_imagery(
  api_key = set_api_key(),
  bbox = my_bbox,
  date_start = "2023-06-01",
  date_end = "2023-06-30",
  cloud_lim = 1,
  ground_control = TRUE,
  quality = "standard"
)
} # }
```
