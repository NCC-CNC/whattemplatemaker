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
  shiny::observeEvent(input$shinyglide_index_main_glide, {
    # specify dependency
    shiny::req(input$shinyglide_index_main_glide)

    # reset nav bar button colors
    shinyBS::updateButton(
      session = session, inputId = "nav_intro_btn", style = "default"
    )
    shinyBS::updateButton(
      session = session, inputId = "nav_site_btn", style = "default"
    )
    shinyBS::updateButton(
      session = session, inputId = "nav_feature_btn", style = "default"
    )
    shinyBS::updateButton(
      session = session, inputId = "nav_action_btn", style = "default"
    )
    shinyBS::updateButton(
      session = session, inputId = "nav_downloads_btn", style = "default"
    )
    # find name of button to update
    screen_id <- c(
      "nav_intro_btn", "nav_site_btn", "nav_feature_btn",
      "nav_action_btn", "nav_downloads_btn"
    )[input$shinyglide_index_main_glide + 1]
    # update nav bar button color to show current stage
    if (!is.na(screen_id)) {
      shinyBS::updateButton(
        session = session, inputId = screen_id, style = "primary"
      )
    }
  })

})
