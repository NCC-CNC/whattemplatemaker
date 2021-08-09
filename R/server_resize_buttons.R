#' Sever function: resize buttons
#'
#' Set behavior for resize buttons.
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
server_resize_buttons <- quote({

  # resize site data
  shiny::observeEvent(input$resize_site_btn, {
    new_nrow <- as.numeric(input$resize_site_input)
    values[["site_data"]] <- resize_table(values[["site_data"]], new_nrow)
    output$site_data_widget <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(
        values[["site_data"]],
        useTypes = TRUE,
        width = 200,
        stretchH = "all"
      )
    })
  })

  # resize action data
  shiny::observeEvent(input$resize_action_btn, {
    new_nrow <- as.numeric(input$resize_action_input)
    values[["action_data"]] <- resize_table(values[["action_data"]], new_nrow)
    output$action_data_widget <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(
        values[["action_data"]],
        useTypes = TRUE,
         width = 200,
         stretchH = "all"
       )
    })
  })

  # resize feature data
  shiny::observeEvent(input$resize_feature_btn, {
    new_nrow <- as.numeric(input$resize_feature_input)
    values[["feature_data"]] <- resize_table(values[["feature_data"]], new_nrow)
    output$feature_data_widget <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(
        values[["feature_data"]],
        useTypes = TRUE,
        width = 200,
        stretchH = "all"
       )
    })
  })

})
