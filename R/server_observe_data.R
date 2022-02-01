#' Sever function: observe data
#'
#' Set behavior for observe data.
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
server_observe_data <- quote({

  # logic for updating user interface when site data updated internally
  shiny::observeEvent(values[["site_data"]], {
    ## disable button to remove rows if only has 1 row
    shinyjs::toggleState(
      id = "site_data_remove_row_btn",
      condition = nrow(values[["site_data"]]) > 1
    )

    ## update table
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

  # logic for updating user interface when feature data updated internally
  shiny::observeEvent(values[["feature_data"]], {
    ## disable button to remove rows if only has 1 row
    shinyjs::toggleState(
      id = "feature_data_remove_row_btn",
      condition = nrow(values[["feature_data"]]) > 1
    )

    ## update table
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

  # store feature data when table widget updated
  shiny::observeEvent(input$feature_data_widget, {
    if (!is.null(input$feature_data_widget)) {
      values[["feature_data"]] <- sanitize_data(
        rhandsontable::hot_to_r(input$feature_data_widget)
      )
    }
  })

  # logic for updating user interface when action data updated internally
  shiny::observeEvent(values[["action_data"]], {
    ## disable button to remove rows if only has 1 row
    shinyjs::toggleState(
      id = "action_data_remove_row_btn",
      condition = nrow(values[["action_data"]]) > 1
    )

    ## update table
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


  # store action data when table widget updated
  shiny::observeEvent(input$action_data_widget, {
    if (!is.null(input$action_data_widget)) {
      values[["action_data"]] <- sanitize_data(
        rhandsontable::hot_to_r(input$action_data_widget)
      )
    }
  })

})
