#' Sever function: site checkbox
#'
#' Set behavior for site checkbox.
#'
#' @details
#' This object is designed to be used within [app_server] function.
#' Within the [app_server] function, it should be called like this:
#'
#' ```
#' eval(server_site_map)
#' ```
#'
#' @noRd
server_site_checkbox <- quote({

  # if user toggles the site checkbox then reset site data and geometry
  shiny::observeEvent(input$site_checkbox, {
    ## dependencies
    if (!any(isTRUE(input$site_checkbox), isFALSE(input$site_checkbox))) {
      return()
    }

    ## update variable to keep track of cleared leaflet_id
    values[["deleted_leaflet_id"]] <- c(
      values[["deleted_leaflet_id"]],
      values[["site_geometry_data"]]$leaflet_id
    )

    ## site data
    if (input$site_checkbox > 0.5) {
      values[["site_data"]] <- initial_site_data
    } else {
      values[["site_data"]] <- resize_table(initial_site_data, 1)
    }

    ## site geometry data
    values[["site_geometry_data"]] <- initial_site_geometry_data

    ## refresh map
    map <-
      leaflet::leafletProxy(site_map_ns("map")) %>%
      leaflet::clearGroup("sites") %>%
      leaflet::clearGeoJSON() %>%
      leaflet.extras::removeDrawToolbar(clearFeatures = TRUE) %>%
      leaflet::addGeoJSON(
        layerId = "sites",
        group = "sites",
        geojson = '{"type": "FeatureCollection", "features": []}'
      )
    do.call(
      leaflet.extras::addDrawToolbar,
      append(
        list(map = map, targetLayerId = "sites"),
        mapedit_opts
      )
    )

  })

})
