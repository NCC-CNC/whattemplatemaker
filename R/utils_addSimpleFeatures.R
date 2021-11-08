#' @include internal.R
NULL

#' Add Simple Features
#'
#' Add simple features data to a [leaflet::leaflet()] map.
#'
#' @param map [leaflet::leaflet()] Map object.
#'
#' @param sf [sf::st_sf()] object.
#'
#' @return An updated [leaflet::leaflet()] map object.
#'
#' @export
addSimpleFeatures <- function(map, sf, ...) {
  # assert arguments are valid
  assertthat::assert_that(inherits(sf, "sf"))

  # just exit if no data
  if (nrow(sf) == 0) return(map)

  # preliminary processing
  pts_idx <- vapply(sf::st_geometry(sf), inherits, logical(1), "POINT")
  shape_idx <- !vapply(
    sf::st_geometry(sf), inherits, logical(1), "GEOMETRYCOLLECTION"
  ) & !pts_idx

  # add points data
  if (any(pts_idx)) {
    leaflet::addMarkers(
      map = map,
      data = sf::as_Spatial(sf[which(pts_idx), , drop = FALSE]),
      ...
    )
  }

  # add shape data
  if (any(shape_idx)) {
    leaflet::addGeoJSON(
       map = map,
       geojson = geojsonsf::sf_geojson(sf[which(shape_idx), , drop = FALSE]),
      ...
    )
  }

  # return map
  map

}
