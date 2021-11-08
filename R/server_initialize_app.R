#' Sever function: initialize application
#'
#' Set behavior for initializing application.
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
server_initialize_app <- quote({

  # initialize data
  values <- shiny::reactiveValues(
    site_data = initial_site_data,
    site_geometry_data = initial_site_geometry_data,
    action_data = initial_action_data,
    feature_data = initial_feature_data
  )

  # disable download button by default
  shinyjs::disable("download_btn")

  # initialize data in table widgets
  shiny::observeEvent(values[["site_data"]], {
    output$site_data_widget <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(
        values[["site_data"]][, c("id", "description"), drop = FALSE],
        useTypes = TRUE,
        width = whattemplatemaker::get_golem_config("table_width"),
        stretchH = "all",
        overflow = "hidden"
      )
    })
  })
  shiny::observeEvent(values[["feature_data"]], {
    output$feature_data_widget <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(
        values[["feature_data"]],
        useTypes = TRUE,
        width = whattemplatemaker::get_golem_config("table_width"),
        stretchH = "all",
        overflow = "hidden"
      )
    })
  })
  shiny::observeEvent(values[["action_data"]], {
    output$action_data_widget <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(
        values[["action_data"]],
        useTypes = TRUE,
        width = whattemplatemaker::get_golem_config("table_width"),
        stretchH = "all",
        overflow = "hidden"
      )
    })
  })

  # initialize data in map edit widgets
  m <- site_leaflet_map()
  site_map_ns <- shiny::NS("site_edit")
  site_module <- shiny::callModule(
    mapedit::editMod,
    "site_edit",
    leafmap = m,
    targetLayerId = "sites",
    editorOptions = mapedit_opts
  )

  # store data
  shiny::observeEvent(input$action_data_widget, {
    if (!is.null(input$action_data_widget)) {
      values[["action_data"]] <- sanitize_data(
        rhandsontable::hot_to_r(input$action_data_widget)
      )
    }
  })
  shiny::observeEvent(input$feature_data_widget, {
    if (!is.null(input$feature_data_widget)) {
      values[["feature_data"]] <- sanitize_data(
        rhandsontable::hot_to_r(input$feature_data_widget)
      )
    }
  })

  # initialize tooltips
  shinyjs::runjs("$('[data-toggle=\"tooltip\"]').tooltip();")

})
