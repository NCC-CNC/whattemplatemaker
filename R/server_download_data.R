#' Sever function: download data
#'
#' Set behavior for downloading data.
#'
#' @details
#' This object is designed to be used within [app_server] function.
#' Within the [app_server] function, it should be called like this:
#'
#' ```
#' eval(server_map)
#' ```
#'
#' @noRd
server_download_data <- quote({

  # download site data
  output$download_template_btn <- shiny::downloadHandler(
    filename = function() {
      "data-template.xlsx"
    },
    contentType = "application/zip",
    content = function(path) {
      ## generate site longitude and latitude data
      if (isTRUE(input$site_checkbox > 0.5)) {
        site_pts <- suppressWarnings(sf::as_Spatial(sf::st_centroid(
          values[["site_geometry_data"]]
        )))
        site_lon <- site_pts@coords[, 1]
        site_lat <- site_pts@coords[, 2]
      } else {
        site_lon <- rep("", length(values[["site_data"]]$id))
        site_lat <- site_lon
      }

      ## process data
      site_ids <- extract_valid_names(
        values[["site_data"]]$id
      )
      site_descriptions <- extract_valid_names(
        values[["site_data"]]$description
      )
      feature_ids <- extract_valid_names(
        values[["feature_data"]]$id
      )
      feature_descriptions <- extract_valid_names(
        values[["feature_data"]]$description
      )
      action_ids <- extract_valid_names(
        values[["action_data"]]$id
      )
      action_descriptions <- extract_valid_names(
        values[["action_data"]]$description
      )

      ## create workbook
      out_data <- whatdataio::create_template_workbook(
        site_ids = site_ids,
        site_descriptions = site_descriptions,
        feature_ids = feature_ids,
        feature_descriptions = feature_descriptions,
        action_ids = action_ids,
        action_descriptions = action_descriptions,
        parameters = parameters,
        site_longitudes = site_lon,
        site_latitudes = site_lat
      )

      ## save data to disk
      openxlsx::saveWorkbook(out_data, path, returnValue = FALSE)
  })

  # download site geometry data
  output$download_spatial_btn <- shiny::downloadHandler(
    filename = function() {
      "data-shapefile.zip"
    },
    contentType = "application/zip",
    content = function(path) {
      ## initialization
      d <- values[["site_data"]]
      s <- values[["site_geometry_data"]]
      if (!isTRUE(input$site_checkbox > 0.5)) return()

      ## prepare data
      out_data <- s
      out_data$id <- d$id
      out_data <- out_data[, "id", drop = FALSE]

      ## save data to disk
      td <- tempfile()
      dir.create(td, showWarnings = TRUE, recursive = TRUE)
      tf <- file.path(td, "data-shapefile.shp")
      sf::write_sf(out_data, tf)

      ## zip data
      withr::with_dir(td, {
        utils::zip(zipfile = path, files = dir(), flags = "-qq")
      })

      ## clean up
      unlink(td, recursive = TRUE, force = TRUE)

  })

})
