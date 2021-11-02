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
#' @export
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

  # return result
  invisible(TRUE)

}
