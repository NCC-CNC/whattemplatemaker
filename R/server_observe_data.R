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
    ## update table
    output$site_data_widget <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(
        values[["site_data"]][, c("id", "description"), drop = FALSE],
        useTypes = TRUE,
        width = whattemplatemaker::get_golem_config("table_width"),
        stretchH = "all"
      ) %>%
      add_hot_col_invalid_id_renderer(
        col = "id",
        nchar = whattemplatemaker::get_golem_config(
          "maximum_id_length"
        )
      ) %>%
      add_hot_col_invalid_desc_renderer(
        col = "description",
        nchar = whattemplatemaker::get_golem_config(
          "maximum_description_length"
        )
      ) %>%
      add_hot_col_nchar_validator(
        col = "id",
        allowInvalid = FALSE,
        nchar = whattemplatemaker::get_golem_config(
          "maximum_id_length"
        )
      ) %>%
      add_hot_col_nchar_validator(
        col = "description",
        allowInvalid = FALSE,
        nchar = whattemplatemaker::get_golem_config(
          "maximum_description_length"
        )
      )
    })

    ## reset row buttons
    shinyFeedback::resetLoadingButton("site_data_add_row_btn")
    shinyFeedback::resetLoadingButton("site_data_remove_row_btn")

    ## disable button to remove rows if only has 1 row
    shinyjs::toggleState(
      id = "site_data_remove_row_btn",
      condition = nrow(values[["site_data"]]) > 1
    )

  })

  # logic for updating user interface when feature data updated internally
  shiny::observeEvent(values[["feature_data"]], {
    ## update table
    output$feature_data_widget <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(
        values[["feature_data"]],
        useTypes = TRUE,
        width = whattemplatemaker::get_golem_config("table_width"),
        stretchH = "all",
        overflow = "hidden"
      ) %>%
      add_hot_col_invalid_id_renderer(
        col = "id",
        nchar = whattemplatemaker::get_golem_config(
          "maximum_id_length"
        )
      ) %>%
      add_hot_col_invalid_desc_renderer(
        col = "description",
        nchar = whattemplatemaker::get_golem_config(
          "maximum_description_length"
        )
      ) %>%
      add_hot_col_nchar_validator(
        col = "id",
        allowInvalid = FALSE,
        nchar = whattemplatemaker::get_golem_config(
          "maximum_id_length"
        )
      ) %>%
      add_hot_col_nchar_validator(
        col = "description",
        allowInvalid = FALSE,
        nchar = whattemplatemaker::get_golem_config(
          "maximum_description_length"
        )
      )
    })

    ## reset row buttons
    shinyFeedback::resetLoadingButton("feature_data_add_row_btn")
    shinyFeedback::resetLoadingButton("feature_data_remove_row_btn")

    ## disable button to remove rows if only has 1 row
    shinyjs::toggleState(
      id = "feature_data_remove_row_btn",
      condition = nrow(values[["feature_data"]]) > 1
    )

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
    ## update table
    output$action_data_widget <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(
        values[["action_data"]],
        useTypes = TRUE,
        width = whattemplatemaker::get_golem_config("table_width"),
        stretchH = "all",
        overflow = "hidden"
      ) %>%
      add_hot_col_invalid_id_renderer(
        col = "id",
        nchar = whattemplatemaker::get_golem_config(
          "maximum_id_length"
        )
      ) %>%
      add_hot_col_invalid_desc_renderer(
        col = "description",
        nchar = whattemplatemaker::get_golem_config(
          "maximum_description_length"
        )
      ) %>%
      add_hot_col_nchar_validator(
        col = "id",
        allowInvalid = FALSE,
        nchar = whattemplatemaker::get_golem_config(
          "maximum_id_length"
        )
      ) %>%
      add_hot_col_nchar_validator(
        col = "description",
        allowInvalid = FALSE,
        nchar = whattemplatemaker::get_golem_config(
          "maximum_description_length"
        )
      )
    })

    ## reset row buttons
    shinyFeedback::resetLoadingButton("action_data_add_row_btn")
    shinyFeedback::resetLoadingButton("action_data_remove_row_btn")

    ## disable button to remove rows if only has 1 row
    shinyjs::toggleState(
      id = "action_data_remove_row_btn",
      condition = nrow(values[["action_data"]]) > 1
    )

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
