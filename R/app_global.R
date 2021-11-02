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
    id = rep(
      NA_character_,
      whattemplatemaker::get_golem_config("n_data_rows")
    ),
    description = rep(
      NA_character_,
      whattemplatemaker::get_golem_config("n_data_rows")
    )
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

})
