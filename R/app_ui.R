#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # add external resources
    golem_add_external_resources(),

    # app content
    shiny::fluidPage(

      ## suppress dependencies that fail to import correctly
      htmltools::suppressDependencies("shinyBS"),

      ## title panel
      shiny::titlePanel("What Template Maker"),

      ## app layout
      shiny::sidebarLayout(
        ## sidebar
        shiny::sidebarPanel(
          shiny::helpText("Welcome to the What Template Maker app! This app is designed to help you prepare input data for the What To Do app. Since the What To Do  requires a lot of data, in a very specific format, we developed this app to help you prepare input data for your study system. Specifically, this app produces a customized template Excel Spreadsheet for entering in your data. Please enter in the relevant information into the tables on the right, and then click the \"Download\" button (below) to save the template Excel Spreadsheet to your computer. After downloading the file, you can then open it on your computer to enter data (e.g. cost data) into the spreadsheet. Once you've finished, you can use the updated spreadsheet with the What To Do app to generate conservation management plans."),
          shiny::br(),
          shiny::helpText("The \"Download\" button will turn blue once you've finished adding information on your study system to the tables. Please note that you can't have any blank cells in the tables, so delete any extra rows. You can add rows to the tables by entering in the desired number of rows into the text box above each table and clicking the \"Resize table\" button. You can also add and remove rows by right clicking on them and selecting the relevant option (e.g. \"Insert row above\" or \"Remove row\")."),
          shiny::br(),
          shiny::div(
            class = "divCenter",
            shiny::downloadButton("download_btn", "Download")
          )
        ),
        ## main panel
        shiny::mainPanel(
          shiny::fluidRow(
            ### site widgets
            shiny::column(
              width = 4,
              shiny::wellPanel(
                shiny::h3("Sites"),
                shiny::helpText("Enter in the names of each site. These correspond to different places that you could potentially manage to improve biodiversity. For example, these values could correspond to the names of places (e.g. \"Vulture Valley\"), database identifiers (e.g. \"12345A\"), or a combination (e.g. \"Vulture Valley (#12345A)\")."),
                shiny::fluidRow(
                  shiny::column(
                    width = 7,
                    shiny::numericInput(
                      "resize_site_input",
                      NULL,
                      width = "100%",
                      value = whattemplatemaker::get_golem_config(
                        "n_data_rows"
                      ),
                      min = 1,
                      max = 10000
                    )
                  ),
                  shiny::column(width = 1,
                    shinyBS::bsButton(
                      "resize_site_btn",
                      "Resize table",
                      style = "primary"
                    )
                  ),
                ),
                rhandsontable::rHandsontableOutput("site_data_widget"),
                shiny::br(),
                shinyBS::bsAlert("site_alert")
              )
            ),
            ### feature widgets
            shiny::column(
              width = 4,
              shiny::wellPanel(
                shiny::h3("Features"),
                shiny::helpText("Enter in the names of each feature. These correspond to different biodiversity elements (e.g. species, habitat types) for which you wish to improve conservation status (e.g. increase population size.) For example, these values could correspond to species names (e.g. \"Caribou\") or vegetation (e.g. \"Alvar\")."),
                shiny::fluidRow(
                  shiny::column(
                    width = 7,
                    shiny::numericInput(
                      "resize_feature_input",
                      NULL,
                      width = "100%",
                      value = whattemplatemaker::get_golem_config(
                        "n_data_rows"
                      ),
                      min = 1,
                      max = 10000
                    )
                  ),
                  shiny::column(
                    width = 1,
                    shinyBS::bsButton(
                      "resize_feature_btn",
                      "Resize table",
                      style = "primary"
                    )
                  ),
                ),
                rhandsontable::rHandsontableOutput("feature_data_widget"),
                shiny::br(),
                shinyBS::bsAlert("feature_alert")
              )
            ),
            ### action widgets
            shiny::column(
              width = 4,
              shiny::wellPanel(
                shiny::h3("Actions"),
                shiny::helpText("Enter in the names of each management action. These correspond to different activities that you could conduct within each site to help the features. For example, actions could include \"forest restoration\" or \"remove emerald ash border\". It is important to include a baseline \"maintenance\" action."),
                shiny::fluidRow(
                  shiny::column(
                    width = 7,
                    shiny::numericInput(
                      "resize_action_input",
                      NULL,
                      width = "100%",
                      value = whattemplatemaker::get_golem_config(
                        "n_data_rows"
                      ),
                      min = 1,
                      max = 10000
                    )
                  ),
                  shiny::column(
                    width = 1,
                    shinyBS::bsButton(
                      "resize_action_btn",
                      "Resize table",
                      style = "primary"
                    )
                  )
                ),
                rhandsontable::rHandsontableOutput("action_data_widget"),
                br(),
                shinyBS::bsAlert("action_alert")
              )
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  # add resources
  add_resource_path(
    "www", app_sys("app/www")
  )

  # define HTML tags in header
  tags$head(
    ## bundle CSS and JS files
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "What To Do"
    ),

    ## favicon
    golem::favicon(),

    ## dependencies
    shinyjs::useShinyjs()
  )
}
