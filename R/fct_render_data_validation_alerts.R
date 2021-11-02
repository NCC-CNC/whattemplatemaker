#' @include internal.R
NULL

#' Render data validation alerts
#'
#' This function displays alerts related to invalid user inputs.
#'
#' @param session Shiny session.
#'
#' @param anchorId `character` Value indicating alert anchor identifier
#'  (used with [shinyBS::createAlert()] and [shinyBS::closeAlert()].
#'
#' @param alertIdPrefix `character` Value indicating prefixes for individual
#'  alerts.
#'
#' @param validation_results `list` Object with validation results
#'  (produced using [validate_data()]).
#'
#' @details
#' This function is used for the side-effect of creating alerts.
#'
#' @return An invisible `TRUE` value.
#'
#' @export
render_data_validation_alerts <- function(session, anchorId,
                                          alertIdPrefix,
                                          validation_results) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.string(anchorId),
    assertthat::noNA(anchorId),
    assertthat::is.string(alertIdPrefix),
    assertthat::noNA(alertIdPrefix),
    is.list(validation_results)
  )

  # create alerts
  for (i in seq_along(validation_results)) {
    ## extract alert information
    curr_id <- paste0(alertIdPrefix, validation_results[[i]]$id)
    ## check status
    if (validation_results[[i]]$success) {
      shinyBS::closeAlert(session = session, alertId = curr_id)
    } else {
      shinyBS::createAlert(
        session = session,
        anchorId = anchorId,
        alertId = curr_id,
        title = validation_results[[i]]$title,
        content = validation_results[[i]]$details,
        append = FALSE
      )
    }
  }

  # return result
  invisible(TRUE)
}
