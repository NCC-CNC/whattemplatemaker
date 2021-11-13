#' @include internal.R
NULL

# Site Leaflet map
#'
#' Initialize a leaflet map for adding site data.
#'
#' @return A [leaflet::leaflet()] object.
#'
#' @export
site_leaflet_map <- function() {
  # create map
  map <-
    ## initialize leaflet map
    leaflet::leaflet() %>%
    ## add basemaps
    leaflet::addProviderTiles(
      leaflet::providers$Esri.WorldImagery,
      group = "Satellite"
    ) %>%
    leaflet::addProviderTiles(
      leaflet::providers$Esri.WorldStreetMap,
      group = "Street view"
    ) %>%
    leaflet::addProviderTiles(
      leaflet::providers$CartoDB.DarkMatter,
      group = "Monochrome"
    ) %>%
    leaflet::addProviderTiles(
      leaflet::providers$CartoDB.Positron,
      group = "Gray scale"
    ) %>%
    ## specify default view window
    leaflet::flyToBounds(
      -165, -30, 165, 60
    ) %>%
    ## set map bounds
    leaflet::setMaxBounds(
      -200, -100, 200, 100
    ) %>%
    ## add basemap controls
    leaflet::addLayersControl(
      baseGroups = c("Satellite", "Street view", "Monochrome", "Gray scale"),
      options = leaflet::layersControlOptions(collapsed = TRUE),
      position = "topright"
    ) %>%
    ## add scale bar
    leaflet::addScaleBar(
      position = "bottomright"
    )

  # remove outdated font awesome dependency
  idx <- vapply(
    map$dependencies,
    FUN.VALUE = logical(1),
    function(x) x$name == "fontawesome" && x$version == "4.7.0"
  )
  map$dependencies <- map$dependencies[which(!idx)]

  # add empty site data
  map <- leaflet::addGeoJSON(
    map = map,
    layerId = "sites",
    group = "sites",
    geojson = '{"type": "FeatureCollection", "features": []}'
  )

  # return map
  map
}
