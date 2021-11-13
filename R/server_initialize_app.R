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
    feature_data = initial_feature_data,
    deleted_leaflet_id = c()
  )

  # disable download button by default
  shinyjs::disable("download_btn")

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

  # initialize tooltips
  shinyjs::runjs("$('[data-toggle=\"tooltip\"]').tooltip();")

})
