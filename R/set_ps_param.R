set_ps <- function(api_key, cloud_lim = 1, item_name = "PSScene", asset = "ortho_analytic_4b_sr", product_bundle = "analytic_sr_udm2", harmonized = F) {
  # (see https://developers.planet.com/docs/data/items-assets/)
  # https://developers.planet.com/docs/apis/data/psscene3-4band-deprecation/

  out <- list(
    api_key = api_key,
    cloud_lim = cloud_lim, # percent from 0-1
    item_name = item_name,
    asset = asset,
    product_bundle = product_bundle,
    harmonized = harmonized
  )

  return(out)
}
