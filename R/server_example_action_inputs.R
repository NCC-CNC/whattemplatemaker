#' Sever function: example action inputs
#'
#' Set behavior select inputs for adding actions based on examples. buttons.
#'
#' @details
#' This object is designed to be used within [app_server] function.
#' Within the [app_server] function, it should be called like this:
#'
#' ```
#' eval(server_example_action_inputs)
#' ```
#'
#' @noRd
server_example_action_inputs <- quote({

  # current action region select input
  shiny::observeEvent(input$action_region_input, {
    # specify dependencies
    shiny::req(input$action_region_input)

    # if region input has nothing selected...
    if (identical(input$action_region_input, "")) {
      ## remove all options from action input
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "action_name_input",
        choices = NULL,
        selected = NULL
      )
      ## disable action input
      shinyjs::disable("action_name_input")
      ## end logic
      return()
    }

    # if something is selected...
    ## determine which actions names should be added to the name picker
    idx <- which(
      example_action_data$region == input$action_region_input
    )
    curr_names <- example_action_data$action[idx]
    ## determine which actions are already present in the table
    curr_selected <- base::intersect(
      curr_names, values[["action_data"]][[1]]
    )

    ## remove all options from action input
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "action_name_input",
      choices = curr_names,
      selected = curr_selected
    )
  })

  # update action name input if new action is entered into the table
  shiny::observeEvent(values[["action_data"]], {
    # specify dependencies
    shiny::req(values[["action_data"]])
    curr_example_names <- input$action_name_input
    if (length(curr_example_names) == 0) {
      return()
    }
    # find out which names currently in the table match the example names
    curr_selected <- base::intersect(
      curr_example_names,
      values[["action_data"]][[1]]
    )
    # update picker if needed
    if (!identical(sort(curr_selected), sort(curr_example_names))) {
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "action_name_input",
        selected = curr_selected
      )
    }
  })

  shiny::observeEvent(input$action_name_input, {
    # specify dependencies
    shiny::req(input$action_name_input)
    # initialize variables
    curr_names <- values[["action_data"]][[1]]
    # find out which existing names need to be removed
    curr_remove_names <- intersect(
      setdiff(example_action_data$action, input$action_name_input),
      curr_names
    )
    # remove names if needed
    if (length(curr_remove_names) > 0) {
      curr_keep_idx <- which(!curr_names %in% curr_remove_names)
      values[["action_data"]][[1]] <- c(
        curr_names[curr_keep_idx],
        rep(
          NA_character_,
          nrow(values[["action_data"]]) - length(curr_keep_idx)
        )
      )
    }
    # find out which new names need to be added
    curr_names <- values[["action_data"]][[1]]
    curr_new_names <- setdiff(input$action_name_input, curr_names)
    # add names if needed
    if (length(curr_new_names) > 0) {
      ## add extra rows to table if needed
      n_extra_rows_needed <- length(curr_new_names) - sum(is.na(curr_names))
      if (n_extra_rows_needed > 0) {
        ### update table
        values[["action_data"]] <- dplyr::bind_rows(
          values[["action_data"]],
          tibble::tibble(name = rep(NA_character_, n_extra_rows_needed))
        )
        ### update number of rows input
        shiny::updateNumericInput(
          session = session,
          inputId = "resize_action_input",
          value = nrow(values[["action_data"]])
        )
      }
      ## add in new names
      na_idx <- which(is.na(values[["action_data"]][[1]]))
      na_idx <- na_idx[seq_along(curr_new_names)]
      values[["action_data"]][[1]][na_idx] <- curr_new_names
    }

  })

})
