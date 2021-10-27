#' Sever function: display information
#'
#' Set behavior for displaying information.
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
server_display_information <- quote({

  # store data
  shiny::observeEvent(input$site_data_widget, {
    if (!is.null(input$site_data_widget)) {
      values[["site_data"]] <- rhandsontable::hot_to_r(
        input$site_data_widget
      )
    }
  })
  shiny::observeEvent(input$action_data_widget, {
    if (!is.null(input$action_data_widget)) {
      values[["action_data"]] <- rhandsontable::hot_to_r(
        input$action_data_widget
      )
    }
  })
  shiny::observeEvent(input$feature_data_widget, {
    if (!is.null(input$feature_data_widget)) {
      values[["feature_data"]] <- rhandsontable::hot_to_r(
        input$feature_data_widget
      )
    }
  })

  # enable/disable button, display helpful information
  shiny::observe({
    # extract objects
    site_data <- values[["site_data"]]
    action_data <- values[["action_data"]]
    feature_data <- values[["feature_data"]]

    # get character limits
    n_char_limit <- whattemplatemaker::get_golem_config("maximum_name_length")

    # verify input data
    has_site_values <- whatdataio::has_valid_values(site_data[[1]])
    has_feature_values <- whatdataio::has_valid_values(feature_data[[1]])
    has_action_values <- whatdataio::has_valid_values(action_data[[1]])
    unique_site_values <- whatdataio::has_unique_values(site_data[[1]])
    unique_feature_values <- whatdataio::has_unique_values(feature_data[[1]])
    unique_action_values <- whatdataio::has_unique_values(action_data[[1]])
    has_short_action_names <- all(
      nchar(as.character(action_data[[1]])) <= n_char_limit, na.rm = TRUE
    )

    # enable/display button
    if (has_site_values && has_feature_values && has_action_values &&
        unique_site_values && unique_feature_values && unique_action_values &&
        has_short_action_names) {
        shinyjs::enable("download_btn")
        shinyjs::addClass("download_btn", "btn-primary")
    } else {
      shinyjs::disable("download_btn")
      shinyjs::removeClass("download_btn", "btn-primary")
    }

    # display warning
    if (!unique_site_values) {
      shinyBS::createAlert(
        session,
        "site_alert",
        "unique_site_alert",
        title = "Oops",
        content = "Each site must have a unique name.",
        append = FALSE
      )
    } else {
      shinyBS::closeAlert(session, "unique_site_alert")
    }
    if (!unique_feature_values) {
      shinyBS::createAlert(
        session,
        "feature_alert",
        "unique_feature_alert",
        title = "Oops",
        content = "Each feature must have a unique name.",
        append = FALSE
      )
    } else {
      shinyBS::closeAlert(session, "unique_feature_alert")
    }
    if (!unique_action_values) {
      shinyBS::createAlert(
        session,
        "action_alert",
        "unique_action_alert",
        title = "Oops",
        content = "Each action must have a unique name.",
        append = FALSE
      )
    } else {
      shinyBS::closeAlert(session, "unique_action_alert")
    }
    if (!has_short_action_names) {
      shinyBS::createAlert(
        session,
        "action_alert",
        "has_short_action_names",
        title = "Oops",
        content = paste0(
          "Each action name must be shorter than ",
          n_char_limit,
          " characters in length."
        ),
        append = FALSE
      )
    } else {
      shinyBS::closeAlert(session, "has_short_action_names")
    }
  })
})
