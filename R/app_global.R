app_global <- quote({

  # set seed for reproducibility
  set.seed(200)

  # load parameters for data handling
  parameters <- RcppTOML::parseTOML(
    system.file("extdata", "config.toml", package = "actionmisc")
  )

  # initialize data
  initial_site_data <- tibble::tibble(
    name = rep(
      NA_character_,
      whattemplatemaker::get_golem_config("n_data_rows")
    )
  )

  initial_action_data <- tibble::tibble(
    name = rep(
      NA_character_,
      whattemplatemaker::get_golem_config("n_data_rows")
    )
  )

  initial_feature_data <- tibble::tibble(
    name = rep(
      NA_character_,
      whattemplatemaker::get_golem_config("n_data_rows")
    )
  )

})
