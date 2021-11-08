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

  # feature data
  ## add row
  shiny::observeEvent(input$feature_data_add_row_btn, {
    values[["feature_data"]] <- resize_table(
      values[["feature_data"]],
      nrow(values[["feature_data"]]) + 1
    )
  })
  ## remove row
  shiny::observeEvent(input$feature_data_remove_row_btn, {
    values[["feature_data"]] <- resize_table(
      values[["feature_data"]],
      nrow(values[["feature_data"]]) - 1
    )
  })

  # action data
  ## add row
  shiny::observeEvent(input$action_data_add_row_btn, {
    values[["action_data"]] <- resize_table(
      values[["action_data"]],
      nrow(values[["action_data"]]) + 1
    )
  })
  ## remove row
  shiny::observeEvent(input$action_data_remove_row_btn, {
    values[["action_data"]] <- resize_table(
      values[["action_data"]],
      nrow(values[["action_data"]]) - 1
    )
  })

})
