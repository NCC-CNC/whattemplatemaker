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

  # define triggers for data validation
  site_data_trigger <- shiny::reactive({
    list(input$shinyglide_index_main_glide, values[["site_data"]])
  })
  feature_data_trigger <- shiny::reactive({
    list(input$shinyglide_index_main_glide, values[["feature_data"]])
  })
  action_data_trigger <- shiny::reactive({
    list(input$shinyglide_index_main_glide, values[["action_data"]])
  })

  # site data validation
  shiny::observeEvent(site_data_trigger(), {
    ## initialization
    shiny::req(input$shinyglide_index_main_glide)
    if (input$shinyglide_index_main_glide != 1) {
      return()
    }

    ## run validation checks
    v <- validate_data(values[["site_data"]])

    ## update navigation buttons if needed
    update_nav_buttons(all(vapply(v, `[[`, logical(1), "success")))

    ## display messages about invalid data
    render_data_validation_alerts(
      session = session,
      anchorId = "site_alert",
      alertIdPrefix = "site",
      validation_results = v
    )
  })

  # feature data validation
  shiny::observeEvent(feature_data_trigger(), {
    ## initialization
    shiny::req(input$shinyglide_index_main_glide)
    if (input$shinyglide_index_main_glide != 1) {
      return()
    }

    ## run validation checks
    v <- validate_data(values[["feature_data"]])

    ## update navigation buttons if needed
    update_nav_buttons(all(vapply(v, `[[`, logical(1), "success")))

    ## display messages about invalid data
    render_data_validation_alerts(
      session = session,
      anchorId = "feature_alert",
      alertIdPrefix = "feature",
      validation_results = v
    )
  })

  # action data validation
  shiny::observeEvent(action_data_trigger(), {
    ## initialization
    shiny::req(input$shinyglide_index_main_glide)
    if (input$shinyglide_index_main_glide != 1) {
      return()
    }

    ## run validation checks
    v <- validate_data(values[["action_data"]])

    ## update navigation buttons if needed
    update_nav_buttons(all(vapply(v, `[[`, logical(1), "success")))

    ## display messages about invalid data
    render_data_validation_alerts(
      session = session,
      anchorId = "action_alert",
      alertIdPrefix = "action",
      validation_results = v
    )
  })

})
