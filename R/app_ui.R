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

    ## suppress dependencies that fail to import correctly
    htmltools::suppressDependencies("shinyBS"),

    # app content
    shiny::fillPage(

      htmltools::tags$div(
        class = "navbar nav-pills navbarPanel",
        shinyBS::bsButton(
          "nav_intro_btn",
          label = "(1) Introduction",
          style = "primary"
        ),
        shiny::icon("chevron-right"),
        shinyBS::bsButton(
          "nav_site_btn",
          label = "(2) Site details",
          style = "default"
        ),
        shiny::icon("chevron-right"),
        shinyBS::bsButton(
          "nav_feature_btn",
          label = "(3) Feature details",
          style = "default"
        ),
        shiny::icon("chevron-right"),
        shinyBS::bsButton(
          "nav_action_btn",
          label = "(4) Action details",
          style = "default"
        ),
        shiny::icon("chevron-right"),
        shinyBS::bsButton(
          "nav_downloads_btn",
          label = "(5) Downloads",
          style = "default"
        )
      ),

      shinyglide::glide(
        id = "main_glide",

        # define screen navigation buttons
        next_label = shiny::icon("arrow-right", class = "fa-3x"),
        previous_label = shiny::icon("arrow-left", class = "fa-3x"),
        custom_controls = htmltools::tags$div(
          class = "glideControlContainer",
          htmltools::tags$div(
            shinyBS::tipify(
              shinyglide::prevButton(),
              title = "Previous section"
            )
          ),
          htmltools::tags$div(
            shinyBS::tipify(
              shinyglide::nextButton(),
              title = "Next section"
            )
          )
        ),

        # introduction
        shinyglide::screen(
          shiny::wellPanel(
            class = "introPanel",
            shiny::h4("Welcome to the What Template Maker app!"),
            shiny::helpText("This app is designed to help you prepare input data for the What To Do app. Since the What To Do requires a lot of data, in a very specific format, we developed this app to help you prepare input data for your planning region. To use this app, please navigate through each of the different steps and fill out the details as needed. Once you have finished you have finished all the steps, you can then download files for the What To Do app."),
            shiny::br(),
            shiny::helpText("To get started, please click the blue arrow in the top right corner of the screen.")
          )
       ),

        # site widgets
        shinyglide::screen(
          shiny::sidebarLayout(
            ## sidebar
            shiny::sidebarPanel(
              width = 4,
              class = "sidebarPanel",
              shiny::h3("Site details"),
              shiny::helpText("Enter in the names of each site. These can include places that you are already managing for conservation. They can also include new places that you might be considering to acquire for future conservation efforts. For example, these values could correspond to the names of places (e.g. \"Vulture Valley\"), database identifiers (e.g. \"12345A\"), or a combination of place names and identifiers (e.g. \"Vulture Valley (#12345A)\"). It is important that each site has a distinctly different name."),
              shiny::br(),
              shiny::helpText("If you need to add or remove rows, you can click on the buttons above the table. You can also right click on a row and select the relevant option (e.g. \"Insert row above\" or \"Remove row\"). Please note that you can't have any blank cells in the table, so any extra rows must be deleted."),
              shiny::br(),
              shiny::helpText("After you have finished entering in these details, please click the blue arrow in the top right corner of the screen.")
            ),
            ## main content
            shiny::div(
              class = "col-sm-7",
              shiny::div(
                class = "mainPanel",
                shiny::div(
                  class = "tableHeader",
                  shiny::h5("Please insert site names in the table below"),
                  shiny::div(
                    class = "tableBtns",
                    shinyBS::tipify(
                      title = "Insert row",
                      shinyBS::bsButton(
                        "site_data_add_row_btn",
                        label = NULL,
                        size = "small",
                        icon = shiny::icon("plus")
                      )
                    ),
                    shinyBS::tipify(
                      title = "Remove row",
                      shinyBS::bsButton(
                        "site_data_remove_row_btn",
                        label = NULL,
                        size = "small",
                        icon = shiny::icon("minus")
                      )
                    )
                  )
                ),
                shiny::div(
                  rhandsontable::rHandsontableOutput("site_data_widget")
                )
              )
            )
          )
        ),

        # feature widgets
        shinyglide::screen(
          shiny::sidebarLayout(
            ## sidebar
            shiny::sidebarPanel(
              width = 4,
              shiny::h3("Feature details"),
              shiny::helpText("Enter in the names of each feature. These correspond to different biodiversity elements (e.g. populations, species, habitat types, forest cover) for which you wish to improve through effective management (e.g. increase population size, increase amount of available habitat). They can also include ecosystem services (e.g. carbon sequestration, human benefits from nature) that you wish to promote (e.g. increase amount of carbon that can be sequestered, increase amount of space for recreational activities). For example, these values could correspond to species names (e.g. \"Caribou\") or vegetation types (e.g. \"Alvar\")."),
              shiny::br(),
              shiny::helpText("If you need to add or remove rows, you can click on the buttons above the table. You can also right click on a row and select the relevant option (e.g. \"Insert row above\" or \"Remove row\"). Please note that you can't have any blank cells in the table, so any extra rows must be deleted."),
              shiny::br(),
              shiny::helpText("After you have finished entering in these details, please click the blue arrow in the top right corner of the screen.")
            ),
            ## main content
            shiny::mainPanel(
              shiny::fluidRow(
                shiny::h5("Please insert feature names in the table below"),
                shinyBS::bsButton(
                  "feature_data_add_row_btn",
                  label = "Add row",
                  icon = shiny::icon("plus")
                ),
                shinyBS::bsButton(
                  "feature_data_remove_row_btn",
                  label = "Remove row",
                  icon = shiny::icon("minus")
                )
              ),
              rhandsontable::rHandsontableOutput("feature_data_widget"),
            )
          )
        ),

        # action widgets
        shinyglide::screen(
          shiny::sidebarLayout(
            ## sidebar
            shiny::sidebarPanel(
              width = 4,
              shiny::h3("Actions details"),
              shiny::helpText("Enter in the names of management actions. These correspond to activities that are designed to help conserve the features. They can include actions that are currently being implemented, and also activities that could be implemented in the future."),
              shiny::br(),
              shiny::helpText("To help you get started with this, we have provided some example management actions. Please select your region below, and then select any of the suggested actions that you think might be relevant for managing the features inside your sites. In addition to these examples, please enter in any further actions that might also be relevant."),
              shiny::br(),
              shinyWidgets::pickerInput(
                inputId = "action_region_input",
                label = NULL,
                choices = unique(example_action_data$region),
                multiple = FALSE,
                options = list(
                  title = "Select your region..."
                )
              ),
              shiny::conditionalPanel(
                "input.action_region_input.length > 0",
                shinyWidgets::pickerInput(
                  inputId = "action_name_input",
                  label = NULL,
                  choices = "",
                  multiple = TRUE,
                  options = list(
                    title = "Select suggested actions...",
                    `selected-text-format` = "count > 3"
                  )
                )
              ),
              shiny::br(),
              shiny::helpText("If you need to add or remove rows, you can click on the buttons above the table. You can also right click on a row and select the relevant option (e.g. \"Insert row above\" or \"Remove row\"). Please note that you can't have any blank cells in the table, so any extra rows must be deleted."),
              shiny::br(),
              shiny::helpText("After you have finished entering in these details, please click the blue arrow in the top right corner of the screen.")
            ),
            ## main content
            shiny::mainPanel(
              shiny::fluidRow(
                shiny::h5("Please insert action names in the table below"),
                shinyBS::bsButton(
                  "action_data_add_row_btn",
                  label = "Add row",
                  icon = shiny::icon("plus")
                ),
                shinyBS::bsButton(
                  "action_data_remove_row_btn",
                  label = "Remove row",
                  icon = shiny::icon("minus")
                )
              ),
              rhandsontable::rHandsontableOutput("action_data_widget"),
            )
          )
        ),

        # download
        shinyglide::screen(
          shiny::wellPanel(
            class = "exportPanel",
            shiny::h3("Downloads"),
            shiny::helpText("Congratulations - you've entered in all the information! Please double check that you've included all the sites, features, and actions; and download the Excel Spreadsheet and spatial (ESRI Shapefile) dataset to your computer. After downloading these files, please open the Excel Spreadsheet and fill in the data as needed (don't worry, the spreadsheet contains instructions for filling in data)."),
            shiny::br(),
            shiny::div(
              class = "divCenter",
              shiny::downloadButton(
                outputId = "download_template_btn",
                label = "Download Excel Spreadsheet"
              ),
              shiny::downloadButton(
                outputId = "download_spatial_btn",
                label = "Download Spatial Dataset"
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
      app_title = "What Template Maker"
    ),

    ## favicon
    golem::favicon(),

    ## dependencies
    shinyjs::useShinyjs()
  )
}
