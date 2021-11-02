#' Sever function: input validation
#'
#' Set behavior for nput validation.
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
server_input_validation <- quote({

  # define trigger for site data validation
  site_data_trigger <- shiny::reactive({
    list(input$shinyglide_index_main_glide, values[["site_data"]])
  })

  # site data validation
  shiny::observeEvent(site_data_trigger(), {
    ## initialization
    shiny::req(input$shinyglide_index_main_glide)
    if (input$shinyglide_index_main_glide != 1) {
      return()
    }

    ## extract data
    n <- values[["site_data"]][[1]]

    ## validation
    site_n_valid <- n_valid_names(n) >= 1
    site_unique_valid <- whatdataio::has_unique_values(n)
    site_nchar_valid <- valid_nchar(
      n, whattemplatemaker::get_golem_config("maximum_name_length")
    )

    ## update navigation buttons if needed
    update_nav_buttons(
      isTRUE(site_n_valid && site_unique_valid && site_nchar_valid)
    )

    ## display messages about invalid data
    if (!site_n_valid) {
      shinyBS::createAlert(
        session = session,
        anchorId = "site_alert",
        alertId = "site_n_alert",
        title = "Oops",
        content = "At least one site is required.",
        append = FALSE
      )
    } else {
      shinyBS::closeAlert(session = session, alertId = "site_n_alert")
    }
    if (!site_unique_valid) {
      shinyBS::createAlert(
        session = session,
        anchorId = "site_alert",
        alertId = "site_unique_alert",
        title = "Oops",
        content = "Each site must have a unique name.",
        append = FALSE
      )
    } else {
      shinyBS::closeAlert(session = session, alertId = "site_unique_alert")
    }
    if (!site_nchar_valid) {
      ### site n
      shinyBS::createAlert(
        session = session,
        anchorId = "site_alert",
        alertId = "site_nchar_alert",
        title = "Oops",
        content = paste0(
          "Each site name must be shorter than ",
          whattemplatemaker::get_golem_config("maximum_name_length"),
          " characters in length."
        ),
        append = FALSE
      )
    } else {
      shinyBS::closeAlert(session = session, "site_nchar_alert")
    }
  })

  # define trigger for site data validation
  feature_data_trigger <- shiny::reactive({
    list(input$shinyglide_index_main_glide, values[["feature_data"]])
  })

  # site data validation
  shiny::observeEvent(feature_data_trigger(), {
    ## initialization
    shiny::req(input$shinyglide_index_main_glide)
    if (input$shinyglide_index_main_glide != 2) {
      return()
    }

    ## extract data
    n <- values[["feature_data"]][[1]]

    ## validation
    feature_n_valid <- n_valid_names(n) >= 1
    feature_unique_valid <- whatdataio::has_unique_values(n)
    feature_nchar_valid <- valid_nchar(
      n, whattemplatemaker::get_golem_config("maximum_name_length")
    )

    ## update navigation buttons if needed
    update_nav_buttons(
      isTRUE(feature_n_valid && feature_unique_valid && feature_nchar_valid)
    )

    ## display messages about invalid data
    if (!feature_n_valid) {
      shinyBS::createAlert(
        session = session,
        anchorId = "feature_alert",
        alertId = "feature_n_alert",
        title = "Oops",
        content = "At least one site is required.",
        append = FALSE
      )
    } else {
      shinyBS::closeAlert(session = session, alertId = "feature_n_alert")
    }
    if (!feature_unique_valid) {
      shinyBS::createAlert(
        session = session,
        anchorId = "feature_alert",
        alertId = "feature_unique_alert",
        title = "Oops",
        content = "Each site must have a unique name.",
        append = FALSE
      )
    } else {
      shinyBS::closeAlert(session = session, alertId = "feature_unique_alert")
    }
    if (!feature_nchar_valid) {
      ### site n
      shinyBS::createAlert(
        session = session,
        anchorId = "feature_alert",
        alertId = "feature_nchar_alert",
        title = "Oops",
        content = paste0(
          "Each site name must be shorter than ",
          whattemplatemaker::get_golem_config("maximum_name_length"),
          " characters in length."
        ),
        append = FALSE
      )
    } else {
      shinyBS::closeAlert(session = session, alertId = "feature_nchar_alert")
    }
  })

  # define trigger for site data validation
  action_data_trigger <- shiny::reactive({
    list(input$shinyglide_index_main_glide, values[["action_data"]])
  })

  # site data validation
  shiny::observeEvent(action_data_trigger(), {
    ## initialization
    shiny::req(input$shinyglide_index_main_glide)
    if (input$shinyglide_index_main_glide != 3) {
      return()
    }

    ## extract data
    n <- values[["action_data"]][[1]]

    ## validation
    action_n_valid <- n_valid_names(n) >= 1
    action_unique_valid <- whatdataio::has_unique_values(n)
    action_nchar_valid <- valid_nchar(
      n, whattemplatemaker::get_golem_config("maximum_name_length")
    )

    ## update navigation buttons if needed
    update_nav_buttons(
      isTRUE(action_n_valid && action_unique_valid && action_nchar_valid)
    )

    ## display messages about invalid data
    if (!action_n_valid) {
      shinyBS::createAlert(
        session = session,
        anchorId = "action_alert",
        alertId = "action_n_alert",
        title = "Oops",
        content = "At least one site is required.",
        append = FALSE
      )
    } else {
      shinyBS::closeAlert(session = session, alertId = "action_n_alert")
    }
    if (!action_unique_valid) {
      shinyBS::createAlert(
        session = session,
        anchorId = "action_alert",
        alertId = "action_unique_alert",
        title = "Oops",
        content = "Each site must have a unique name.",
        append = FALSE
      )
    } else {
      shinyBS::closeAlert(session = session, alertId = "action_unique_alert")
    }
    if (!action_nchar_valid) {
      ### site n
      shinyBS::createAlert(
        session = session,
        anchorId = "action_alert",
        alertId = "action_nchar_alert",
        title = "Oops",
        content = paste0(
          "Each site name must be shorter than ",
          whattemplatemaker::get_golem_config("maximum_name_length"),
          " characters in length."
        ),
        append = FALSE
      )
    } else {
      shinyBS::closeAlert(session = session, alertId = "action_nchar_alert")
    }
  })

})
