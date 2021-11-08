app_global <- quote({

  # set seed for reproducibility
  set.seed(200)

  # load parameters for data handling
  parameters <- whatdataio::read_data_configuration()

  # load example actions
  example_action_data <- read.table(
    system.file(
      "extdata", "example-actions.csv", package = "whattemplatemaker"
    ),
    header = TRUE,
    stringsAsFactors = FALSE,
    sep = ","
  )

  # verify that example action names conform to limit
  ## id
  invalid_example_idx <- which(
    nchar(example_action_data$id) >
    whattemplatemaker::get_golem_config("maximum_id_length")
  )
  assertthat::assert_that(
    length(invalid_example_idx) == 0,
    msg = paste(
      "the following example ids are too long:",
      paste(
        paste0("\"", example_action_data$id[invalid_example_idx], "\""),
        collapse = ", "
      )
    )
  )
  assertthat::assert_that(
    anyDuplicated(example_action_data$id) == 0L,
    msg = "cannot have duplicated ids"
  )
  ## description
  invalid_example_idx <- which(
    nchar(example_action_data$description) >
    whattemplatemaker::get_golem_config("maximum_description_length")
  )
  assertthat::assert_that(
    length(invalid_example_idx) == 0,
    msg = paste(
      "the following example descriptions are too long:",
      paste(
        paste0(
          "\"", example_action_data$description[invalid_example_idx], "\""
        ),
        collapse = ", "
      )
    )
  )

  # initialize data
  initial_site_data <- tibble::tibble(
    leaflet_id = character(0),
    id = character(0),
    description = character(0)
  )

  initial_action_data <- tibble::tibble(
    id = rep(
      NA_character_,
      whattemplatemaker::get_golem_config("n_data_rows")
    ),
    description = rep(
      NA_character_,
      whattemplatemaker::get_golem_config("n_data_rows")
    )
  )

  initial_feature_data <- tibble::tibble(
    id = rep(
      NA_character_,
      whattemplatemaker::get_golem_config("n_data_rows")
    ),
    description = rep(
      NA_character_,
      whattemplatemaker::get_golem_config("n_data_rows")
    )
  )

  initial_site_geometry_data <- sf::st_sf(
    leaflet_id = character(0),
    id = character(0),
    description = character(0),
    geometry = sf::st_sfc(crs = 4326)
  )

  mapedit_opts <- list(
    polylineOptions = FALSE,
    polygonOptions = leaflet.extras::drawPolygonOptions(
      shapeOptions = leaflet.extras::drawShapeOptions(,
        color = whattemplatemaker::get_golem_config("site_color"),
        fillColor = whattemplatemaker::get_golem_config("site_fill_color")
      )
    ),
    circleOptions = FALSE,
    rectangleOptions = FALSE,
    markerOptions = FALSE,
    circleMarkerOptions = FALSE,
    editOptions = leaflet.extras::editToolbarOptions(
      edit = TRUE, remove = TRUE, selectedPathOptions = NULL,
      allowIntersection = TRUE
    )
  )

})
