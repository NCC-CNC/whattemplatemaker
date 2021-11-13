#' Sever function: glide
#'
#' Set behavior for glide.
#'
#' @details
#' This object is designed to be used within [app_server] function.
#' Within the [app_server] function, it should be called like this:
#'
#' ```
#' eval(server_glide)
#' ```
#'
#' @noRd
server_glide <- quote({

  # update nav bar to display current stage
  shiny::observeEvent(input$shinyglide_index_main_glide, {
    # specify dependency
    shiny::req(input$shinyglide_index_main_glide)

    # reset nav bar button colors
    shinyjs::removeClass(
      selector = ".navbar .label-light",
      class = "label-light"
    )

    # find name of button to update
    screen_id <- c(
      "nav_intro_btn", "nav_site_btn", "nav_feature_btn",
      "nav_action_btn", "nav_downloads_btn"
    )[input$shinyglide_index_main_glide + 1]

    # update nav bar button color to show current stage
    if (!is.na(screen_id)) {
      shinyjs::addClass(
        id = screen_id,
        class = "label-light"
      )
    }
  })

})
