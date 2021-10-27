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
    shiny::navbarPage(
      title = "What Template Maker",

      # site widgets
      shiny::tabPanel(
        title = "(1) Enter site details",
        shiny::sidebarLayout(
          ## sidebar
          shiny::sidebarPanel(
            width = 4,
            shiny::h3("Site details"),
            shiny::helpText("Enter in the names of each site. These can include places that you are already managing for conservation. They can also include new places that you might be considering to acquire for future conservation efforts. For example, these values could correspond to the names of places (e.g. \"Vulture Valley\"), database identifiers (e.g. \"12345A\"), or a combination of place names and identifiers (e.g. \"Vulture Valley (#12345A)\"). It is important that each site has a distinctly different name."),
            shiny::br(),
            shiny::helpText("If you need to add or remove rows, you can click on the buttons above the table. You can also right click on a row and select the relevant option (e.g. \"Insert row above\" or \"Remove row\"). Please note that you can't have any blank cells in the table, so any extra rows must be deleted."),
            shiny::br(),
            shiny::helpText("After you have finished entering in these details, please select the next section in the navigation bar at the top of page. Please note that you can revisit this section later by clicking on it in the navigation bar.")
          ),
          ## main content
          shiny::mainPanel(
            shiny::fluidRow(
              shiny::h5("Please insert site names in the table below"),
              shinyBS::bsButton(
                "feature_data_add_row_btn",
                label = "Add row",
                icon = shiny::icon("fa-plus")
              ),
              shinyBS::bsButton(
                "feature_data_remove_row_btn",
                label = "Remove row",
                icon = shiny::icon("fa-minus")
              )
            ),
            rhandsontable::rHandsontableOutput("site_data_widget")
          )
        )
      ),

      # feature widgets
      shiny::tabPanel(
        title = "(2) Enter feature details",
        shiny::sidebarLayout(
          ## sidebar
          shiny::sidebarPanel(
            width = 4,
            shiny::h3("Feature details"),
            shiny::helpText("Enter in the names of each feature. These correspond to different biodiversity elements (e.g. populations, species, habitat types, forest cover) for which you wish to improve through effective management (e.g. increase population size, increase amount of available habitat). They can also include ecosystem services (e.g. carbon sequestration, human benefits from nature) that you wish to promote (e.g. increase amount of carbon that can be sequestered, increase amount of space for recreational activities). For example, these values could correspond to species names (e.g. \"Caribou\") or vegetation types (e.g. \"Alvar\")."),
            shiny::br(),
            shiny::helpText("If you need to add or remove rows, you can click on the buttons above the table. You can also right click on a row and select the relevant option (e.g. \"Insert row above\" or \"Remove row\"). Please note that you can't have any blank cells in the table, so any extra rows must be deleted."),
            shiny::br(),
            shiny::helpText("After you have finished entering in these details, please select the next section in the navigation bar at the top of page. Please note that you can revisit this section later by clicking on it in the navigation bar.")
          ),
          ## main content
          shiny::mainPanel(
            shiny::fluidRow(
              shiny::h5("Please insert feature names in the table below"),
              shinyBS::bsButton(
                "feature_data_add_row_btn",
                label = "Add row",
                icon = shiny::icon("fa-plus")
              ),
              shinyBS::bsButton(
                "feature_data_remove_row_btn",
                label = "Remove row",
                icon = shiny::icon("fa-minus")
              )
            ),
            rhandsontable::rHandsontableOutput("feature_data_widget"),
          )
        )
      ),

      # action widgets
      shiny::tabPanel(
        title = "(3) Enter action details",
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
            shiny::helpText("After you have finished entering in these details, please select the next section in the navigation bar at the top of page. Please note that you can revisit this section later by clicking on it in the navigation bar.")
          ),
          ## main content
          shiny::mainPanel(
            shiny::fluidRow(
              shiny::h5("Please insert action names in the table below"),
              shinyBS::bsButton(
                "action_data_add_row_btn",
                label = "Add row",
                icon = shiny::icon("fa-plus")
              ),
              shinyBS::bsButton(
                "action_data_remove_row_btn",
                label = "Remove row",
                icon = shiny::icon("fa-minus")
              )
            ),
            rhandsontable::rHandsontableOutput("action_data_widget"),
          )
        )
      ),

      # download
      shiny::tabPanel(
        title = "(4) Downloads",
        shiny::sidebarLayout(
          ## sidebar
          shiny::sidebarPanel(
            width = 4,
            shiny::h3("Downloads"),
            shiny::helpText("Congratulations - you've entered in all the information for this  to create a templa needed to create a

            After entering in the details for your sites, features, and actions; please click the download buttons to download (1) a template Excel spreadsheet and (2) a spatial data (ESRI Shapefile) file containing the location of your sites."),
            ),
            shiny::br(),
            shiny::helpText("


            ## main content
          shiny::mainPanel(
            "The \"Download\" button will turn blue once you've finished adding information on your study system to the tables. Please note that you can't have any blank cells in the tables, so delete any extra rows. You can add rows to the tables by entering in the desired number of rows into the text box above each table and clicking the \"Resize table\" button. You can also add and remove rows by right clicking on them and selecting the relevant option (e.g. \"Insert row above\" or \"Remove row\")."),
          shiny::br(),

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
