#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # initialize application
  eval(server_initialize_app)

  # navigation buttons
  eval(server_glide)

  # example action inputs
  eval(server_example_action_inputs)

  # input validation
  eval(server_input_validation)

  # resize buttons
  eval(server_resize_buttons)

  # download data
  eval(server_download_data)

}
