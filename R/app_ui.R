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
              shiny::helpText("Please enter details for each site. Specifically, sites can include places (or areas) that you are already managing for conservation. They can also include new places (or areas) that you might be considering to acquire for future conservation efforts. For each site, please input a unique identifier (\"id\" column) and a description ( \"description\" column)."),
              shiny::helpText("The identifiers should provide a short, distinct, memorable name for each site that allows you to easily distinguish between the different sites. For example, they could be specified using short names (e.g., \"Vulture Valley\"), database codes (e.g., \"12345A\"), or a combination of short names and database codes (e.g., \"Vulture Valley (#12345A)\")."),
              shiny::helpText("The descriptions are used to record additional information on each site. Ideally, this information should provide context to help understand each site could be important for conservation management. For instance, the description could discuss presence of iconic taxa, or provisioning of ecosystem services."),
              shiny::helpText("To add a new site, please click on the draw button (with the", htmltools::tags$img(src = "www/polygon.svg", style = "display:inline;height:0.8em"), "icon), sketch out the spatial boundary for the site, and then fill in the table with an identifier and description for the site. If you do not wish to draw the site boundaries, you can click the checkbox above the map and enter identifiers and descriptions for the sites directly."),
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
              shiny::helpText("Please enter details of each feature. These correspond to different biodiversity elements (e.g., populations, species, habitat types, forest cover) for which you wish to improve through effective management (e.g., increase population size, increase amount of available habitat). They can also include ecosystem services (e.g., carbon sequestration, human benefits from nature) that you wish to promote (e.g., increase amount of carbon that can be sequestered, increase amount of space for recreational activities). For each feature, please input a unique identifier (\"id\" column) and a description ( \"description\" column)."),
              shiny::helpText("The identifiers should provide a short, distinct, memorable name for each feature that allows you to easily distinguish between the different feature. For example, they could be specified using common names (e.g., \"Caribou\", \"Intact alvar\"), scientific names (e.g., \"Rangifer tarandus\"), database codes (e.g., \"12345A\"), or a combination."),
              shiny::helpText("The descriptions are used to record additional information about each feature. Ideally, this information should provide context to help understand why the feature is important for conservation management. For instance, the description could describe threat status (e.g., recorded as Critically Endangered on the IUCN Red List), protection by legislation (e.g., protected by federal-level environmental legislation), or the benefits it provides to humans (e.g., provides carbon sequestration). This information can also describe the consequence data that will be entered after downloading the template. For example, this information can be used to make a note that the data should denote record the presence/absence, population size (number of individuals), or amount of suitable habitat (hectares of land) for a particular species."),
              shiny::helpText("If you need to add or remove rows, you can click on the buttons above the table. You can also right click on a row and select the relevant option (e.g., \"Insert row above\" or \"Remove row\")."),
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
              shiny::helpText("Please enter details for each management action. These correspond to conservation activities that can be implemented in sites to help the features. They can include actions that are currently being implemented within particular sites, and also activities that could be implemented in the future. To obtain useful recommendations, it is important that each action has a level of specificity that reflects real-world implementation. For example, if a site has multiple different invasive species and each invasive species requires different management actions (e.g., some may requiring traps, and others may require shooting), then it would be important to have separate actions for each invasive species (e.g., \"trap rats\" and \"shoot goats\"). For each action, please input a unique identifier (\"id\" column) and a description ( \"description\" column)."),
              shiny::helpText("The identifiers should provide a short, distinct, memorable name for each action that allows you to easily distinguish between the different action. For example, they could be specified using short names (e.g., \"plant trees\", \"controlled burns\", \"trap rats\", \"shoot goats\"), database codes (e.g., \"12345A\"), or a combination of them."),
              shiny::helpText("The descriptions are used to record additional information about each action. Ideally, this information should provide context to help understand why the action might be an useful for conservation management. For instance, the descriptions could detail how the action has previously been implemented in the study area, which features are most likely to benefit from the action, and any feasibility issues with implementing the action."),
              shiny::helpText("To help you get started with this, we have provided some example management actions that can be added using the drop-down menu."),
              shiny::helpText("If you need to add or remove rows, you can click on the buttons above the table. You can also right click on a row and select the relevant option (e.g., \"Insert row above\" or \"Remove row\")."),
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
                      title = "Add suggested actions...",
                      `selected-text-format` = "count > 3"
                    )
                  ),
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
