#' @include internal.R
NULL

#' Update navigation buttons
#'
#' Enable or disable navigation buttons
#'
#' @param x `logical` Indicating if buttons should be enabled, or disabled.
#'
#' @return Invisible `TRUE`.
#'
#' @noRd
update_nav_buttons <- function(x) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.flag(x),
    assertthat::noNA(x)
  )

  # update screen buttons
  shinyjs::toggleState(
    condition = isTRUE(x),
    selector = ".glideControlContainer .btn"
  )

  # remove all active tooltips to avoid stuck tooltips on disabled elements
  shinyjs::runjs("$('[data-toggle=\"tooltip\"]').tooltip('hide');")

  # return result
  invisible(TRUE)

}
