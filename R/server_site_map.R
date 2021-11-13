#' Sever function: site map
#'
#' Set behavior for site map.
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
server_site_map <- quote({

  # update site geometry data when user adds new data
  shiny::observeEvent(site_module()$finished, {
    ## extract data
    shiny::req(site_module()$finished)
    x <- site_module()$finished
    y <- site_module()$deleted
    z <- values[["deleted_leaflet_id"]]

    ## don't update data if map is hidden
    if (!isTRUE(input$site_checkbox)) return()

    ## update column name
    names(x)[1] <- "leaflet_id"
    x$leaflet_id <- as.character(x$leaflet_id)

    ## delete geometries as needed
    w <- z
    if (!is.null(y)) {
      w <- c(w, y[[1]])
    }
    if (length(w) > 0) {
      x <- x[!x[[1]] %in% w, , drop = FALSE]
    }

    ## update data
    values[["site_geometry_data"]] <- x

  })

  # remove site geometry data when user remove data
  shiny::observeEvent(site_module()$deleted, {
    ## extract data
    shiny::req(site_module()$deleted)
    x <- values[["site_geometry_data"]]
    y <- site_module()$deleted
    z <- values[["deleted_leaflet_id"]]

    ## don't update data if map is hidden
    if (!isTRUE(input$site_checkbox)) return()

    ## delete geometries as needed
    w <- z
    if (!is.null(y)) {
      w <- c(w, y[[1]])
    }
    if ((length(w) > 0) && !is.null(y)) {
      x <- x[!x[[1]] %in% w, , drop = FALSE]
      values[["site_geometry_data"]] <- x
    }
  })

  # update site data based on changes to geometry data
  shiny::observeEvent(values[["site_geometry_data"]], {
    ## initialization
    shiny::req(values[["site_geometry_data"]])
    d <- values[["site_data"]]
    s <- values[["site_geometry_data"]]

    ## exit if no changes needed
    if (identical(d$leaflet_id, s$leaflet_id)) return()

    ## don't update data if map is hidden
    if (!isTRUE(input$site_checkbox)) return()

    ## remove sites if needed
    if (is.null(s) || nrow(s) == 0) {
      ### if removing all sites
      d <- d[0, , drop = FALSE]
    } else if ((nrow(d) > 0)) {
      ### if only removing some sites
      idx <- which(d$leaflet_id %in% s$leaflet_id)
      d <- d[idx, , drop = FALSE]
    }

    ## add new sites if needed
    if (!is.null(s) && nrow(s) > 0) {
      idx <- which(!s$leaflet_id %in% d$leaflet_id)
      if (length(idx) > 0) {
        d <- dplyr::bind_rows(
          d,
          tibble::tibble(
            leaflet_id = s$leaflet_id[idx],
            id = NA_character_,
            description = NA_character_
          )
        )
      }
    }

    ## update data
    values[["site_data"]] <- d
  })

  # update map based on site data
  shiny::observeEvent(input$site_data_widget, {
    ## initialization
    shiny::req(input$site_data_widget)
    shiny::req(values[["site_geometry_data"]])

    ## update site data
    d <- values[["site_data"]]
    if (nrow(d) == 0) return() # manually exit if empty
    d2 <- sanitize_data(
      rhandsontable::hot_to_r(input$site_data_widget)
    )
    d$id <- d2$id
    d$description <- d2$description
    values[["site_data"]] <- d

    ## don't update geometry if map is hidden
    if (!isTRUE(input$site_checkbox)) return()

    ## update site geometry data
    s <- values[["site_geometry_data"]]
    idx <- match(s$leaflet_id, d$leaflet_id)
    s <- s[idx, , drop = FALSE]

    ## update map if needed
    if (!identical(s$popup, d$id)) {
      ### update popup
      s$popup <- d$id
      ### update map
      map <-
        leaflet::leafletProxy(site_map_ns("map")) %>%
        leaflet::clearGroup("sites") %>%
        leaflet::clearGeoJSON() %>%
        leaflet.extras::removeDrawToolbar(clearFeatures = TRUE) %>%
        addSimpleFeatures(
          sf = s,
          layerId = "sites",
          group = "sites",
          color = whattemplatemaker::get_golem_config("site_color"),
          fillColor = whattemplatemaker::get_golem_config("site_fill_color")
        )
      do.call(
        leaflet.extras::addDrawToolbar,
        append(
          list(map = map, targetLayerId = "sites"),
          mapedit_opts
        )
      )
      ### update reactive value
      values[["site_geometry_data"]] <- s
    }

  })

})
