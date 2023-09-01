set_ps <- function(api_key) {
  # (see https://developers.planet.com/docs/data/items-assets/)
  # https://developers.planet.com/docs/apis/data/psscene3-4band-deprecation/

  out <- list(
    api_key = api_key,
    cloud_lim = 1, # percent from 0-1
    item_name = "PSScene",
    asset = "ortho_analytic_4b_sr",
    product_bundle = "analytic_sr_udm2"
  )

  return(out)
}
