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
      out_data <- whatdataio::create_template_workbook(
        site_names = values[["site_data"]][[1]],
        feature_names = values[["feature_data"]][[1]],
        action_names = values[["action_data"]][[1]],
        parameters = parameters)
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
