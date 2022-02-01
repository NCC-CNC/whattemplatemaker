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

  # update action name input if new action is entered into the table
  shiny::observeEvent(values[["action_data"]], {
    ## specify dependencies
    shiny::req(values[["action_data"]])
    curr_example_ids <- input$action_id_input
    if (length(curr_example_ids) == 0) {
      return()
    }

    ## find out which names currently in the table match the example names
    curr_selected_ids <- base::intersect(
      curr_example_ids,
      values[["action_data"]]$id
    )

    ## update picker if needed
    if (!identical(sort(curr_selected_ids), sort(curr_example_ids))) {
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "action_id_input",
        selected = curr_selected_ids
      )
    }
  })

  # update action name in table if option is selected in drop down menu
  shiny::observeEvent(input$action_id_input, {
    ## specify dependencies
    shiny::req(input$action_id_input)

    ## initialize variables
    curr_ids <- values[["action_data"]]$id

    ## find out which existing names need to be removed
    curr_remove_ids <- intersect(
      setdiff(example_action_data$id, input$action_id_input),
      curr_ids
    )

    ## remove names if needed
    if (length(curr_remove_ids) > 0) {
      curr_keep_idx <- which(!curr_ids %in% curr_remove_ids)
      values[["action_data"]][[1]] <- c(
        curr_ids[curr_keep_idx],
        rep(
          NA_character_,
          nrow(values[["action_data"]]) - length(curr_keep_idx)
        )
      )
    }

    ## find out which new names need to be added
    curr_ids <- values[["action_data"]][[1]]
    curr_new_ids <- setdiff(input$action_id_input, curr_ids)
    curr_new_descriptions <- example_action_data$description[
      match(curr_new_ids, example_action_data$id)
    ]
    ## add names if needed
    if (length(curr_new_ids) > 0) {
      ## add extra rows to table if needed
      n_extra_rows_needed <- length(curr_new_ids) - sum(is.na(curr_ids))
      if (n_extra_rows_needed > 0) {
        ### update table
        values[["action_data"]] <- dplyr::bind_rows(
          values[["action_data"]],
          tibble::tibble(
            id = rep(NA_character_, n_extra_rows_needed),
            description = rep(NA_character_, n_extra_rows_needed)
          )
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
      na_idx <- na_idx[seq_along(curr_new_ids)]
      values[["action_data"]]$id[na_idx] <- curr_new_ids
      values[["action_data"]]$description[na_idx] <- curr_new_descriptions
    }

  })

})
