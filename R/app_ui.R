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
        class = "navbar",
        htmltools::tags$h3(
          htmltools::tags$div(
            class = "navbarPanel",
            htmltools::tags$span(
              id = "nav_intro_btn",
              class = "label label-default label-light",
              "(1) Introduction"
            ),
            shiny::icon("chevron-right"),
            htmltools::tags$span(
              id = "nav_site_btn",
              class = "label label-default",
              "(2) Site details"
            ),
            shiny::icon("chevron-right"),
            htmltools::tags$span(
              id = "nav_feature_btn",
              class = "label label-default",
              "(3) Feature details"
            ),
            shiny::icon("chevron-right"),
            htmltools::tags$span(
              id = "nav_action_btn",
              class = "label label-default",
              "(4) Action details"
            ),
            shiny::icon("chevron-right"),
            htmltools::tags$span(
              id = "nav_downloads_btn",
              class = "label label-default",
              "(5) Downloads"
            )
          )
        )
      ),

      shinyglide::glide(
        id = "main_glide",

        # define screen navigation buttons
        next_label = shiny::icon("arrow-right", class = "fa-3x"),
        previous_label = shiny::icon("arrow-left", class = "fa-3x"),
        keyboard = FALSE,
        custom_controls = htmltools::tags$div(
          class = "glideControlContainer",
          htmltools::tags$div(
            `data-toggle` = "tooltip",
            `data-placement` = "bottom",
            `data-trigger` = "hover",
            title = "Previous section",
            shinyglide::prevButton(),
          ),
          htmltools::tags$div(
            `data-toggle` = "tooltip",
            `data-placement` = "bottom",
            `data-trigger` = "hover",
            title = "Previous section",
            shinyglide::nextButton(),
            title = "Next section"
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
              shiny::helpText("If you need to add or remove rows, you can click on the buttons above the table. You can also right click on a row and select the relevant option (e.g. \"Insert row above\" or \"Remove row\")."),
              shiny::br(),
              shiny::helpText("After you have finished entering in these details, please click the blue arrow in the top right corner of the screen.")
            ),
            ## main content
            shiny::div(
              class = "col-sm-7",
              shiny::div(
                class = "mainPanel",
                ## header
                shiny::div(
                  class = "tableHeader",
                  shiny::h5("Please insert site details below"),
                  shiny::checkboxInput(
                    "site_checkbox",
                    label = "draw spatial boundaries for sites?",
                    value = TRUE
                  ),
                  shiny::div(
                    class = "tableBtnsContainer",
                    shiny::conditionalPanel(
                      condition = "input.site_checkbox < 0.5",
                      shiny::div(
                        class = "tableHeader",
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
                      )
                    )
                  )
                ),
                ## panel for adding sites with spatial data
                shiny::conditionalPanel(
                  condition = "input.site_checkbox > 0.5",
                  mapedit::editModUI("site_edit"),
                  shiny::br(),
                  shiny::div(
                    rhandsontable::rHandsontableOutput(
                      "site_spatial_data_widget"
                    )
                  )
                ),
                ## table
                shiny::div(
                  rhandsontable::rHandsontableOutput("site_data_widget")
                ),
                ## alert
                shiny::br(),
                shiny::div(
                  shinyBS::bsAlert(anchorId = "site_alert")
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
              class = "sidebarPanel",
              shiny::h3("Feature details"),
              shiny::helpText("Enter in the names of each feature. These correspond to different biodiversity elements (e.g. populations, species, habitat types, forest cover) for which you wish to improve through effective management (e.g. increase population size, increase amount of available habitat). They can also include ecosystem services (e.g. carbon sequestration, human benefits from nature) that you wish to promote (e.g. increase amount of carbon that can be sequestered, increase amount of space for recreational activities). For example, these values could correspond to species names (e.g. \"Caribou\") or vegetation types (e.g. \"Alvar\")."),
              shiny::br(),
              shiny::helpText("If you need to add or remove rows, you can click on the buttons above the table. You can also right click on a row and select the relevant option (e.g. \"Insert row above\" or \"Remove row\")."),
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
                  shiny::h5("Please insert feature details in the table below"),
                  shiny::div(
                    class = "tableBtns",
                    shinyBS::tipify(
                      title = "Insert row",
                      shinyBS::bsButton(
                        "feature_data_add_row_btn",
                        label = NULL,
                        size = "small",
                        icon = shiny::icon("plus")
                      )
                    ),
                    shinyBS::tipify(
                      title = "Remove row",
                      shinyBS::bsButton(
                        "feature_data_remove_row_btn",
                        label = NULL,
                        size = "small",
                        icon = shiny::icon("minus")
                      )
                    )
                  )
                ),
                shiny::div(
                  rhandsontable::rHandsontableOutput("feature_data_widget")
                ),
                shiny::br(),
                shiny::div(
                  shinyBS::bsAlert(anchorId = "feature_alert")
                )
              )
            )
          )
        ),

        # action widgets
        shinyglide::screen(
          shiny::sidebarLayout(
            ## sidebar
            shiny::sidebarPanel(
              width = 4,
              class = "sidebarPanel",
              shiny::h3("Actions details"),
              shiny::helpText("Enter in the names of management actions. These correspond to activities that are designed to help conserve the features. They can include actions that are currently being implemented, and also activities that could be implemented in the future."),
              shiny::br(),
              shiny::helpText("To help you get started with this, we have provided some example management actions. Please have a look through the example actions and select that you think might be relevant for managing the features inside your sites. In addition to these examples, please enter into the table any other actions that might also be relevant."),
              shiny::br(),
              shinyWidgets::pickerInput(
                inputId = "action_id_input",
                label = NULL,
                choices =
                stats::setNames(
                  lapply(
                    sort(unique(example_action_data$ecosystem)),
                    function(x) {
                      i <- example_action_data$ecosystem == x
                      sort(example_action_data$id[i])
                    }
                  ),
                  sort(unique(example_action_data$ecosystem))
                ),
                multiple = TRUE,
                options = list(
                  title = "Select suggested actions...",
                  `selected-text-format` = "count > 3"
                )
              ),
              shiny::br(),
              shiny::helpText("If you need to add or remove rows, you can click on the buttons above the table. You can also right click on a row and select the relevant option (e.g. \"Insert row above\" or \"Remove row\")."),
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
                  shiny::h5("Please insert action details in the table below"),
                  shiny::div(
                    class = "tableBtns",
                    shinyBS::tipify(
                      title = "Insert row",
                      shinyBS::bsButton(
                        "action_data_add_row_btn",
                        label = NULL,
                        size = "small",
                        icon = shiny::icon("plus")
                      )
                    ),
                    shinyBS::tipify(
                      title = "Remove row",
                      shinyBS::bsButton(
                        "action_data_remove_row_btn",
                        label = NULL,
                        size = "small",
                        icon = shiny::icon("minus")
                      )
                    )
                  )
                ),
                shiny::div(
                  rhandsontable::rHandsontableOutput("action_data_widget")
                ),
                shiny::br(),
                shiny::div(
                  shinyBS::bsAlert(anchorId = "action_alert")
                )
              )
            )
          )
        ),

        # download
        shinyglide::screen(
          shiny::wellPanel(
            class = "exportPanel",
            shiny::h3("Downloads"),
            shiny::conditionalPanel(
              condition = "input.site_checkbox > 0.5",
              shiny::helpText("Congratulations - you've entered in all the information! Please double check that you've included all the sites, features, and actions. Next, click the buttons below to download an Excel Spreadsheet and spatial (ESRI Shapefile) dataset to your computer. After downloading these files, please open the Excel Spreadsheet and follow the instructions inside it to enter in your data.")
            ),
            shiny::conditionalPanel(
              condition = "input.site_checkbox < 0.5",
              shiny::helpText("Congratulations - you've entered in all the information! Please double check that you've included all the sites, features, and actions. Next, click the button below to download an Excel Spreadsheet to your computer. After downloading the file, please open it and follow the instructions inside it to enter in your data.")
            ),
            shiny::br(),
            shiny::div(
              class = "exportButtons",
              shiny::downloadButton(
                outputId = "download_template_btn",
                label = "Download Excel Spreadsheet"
              ),
              shiny::conditionalPanel(
                condition = "input.site_checkbox > 0.5",
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

    ## dependencies
    shinyjs::useShinyjs()
  )
}
