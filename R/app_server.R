#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # initialize application
  eval(server_initialize_app)

  # example action inputs
  eval(server_example_action_inputs)

  # display information
  eval(server_display_information)

  # resize buttons
  eval(server_resize_buttons)

  # download data
  eval(server_download_data)

}
