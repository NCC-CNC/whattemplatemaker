#' @include internal.R
NULL

#' Sanitize data
#'
#' Sanitize data input by a user.
#'
#' @param x `data.frame` Object.
#'
#' @return A `data.frame` object.
#'
#' @details
#' At the moment, this just replaces
#' zero-length character values with missing (`NA`) values.
#'
#' @noRd
sanitize_data <- function(x) {
  assertthat::assert_that(inherits(x, "data.frame"))
  x <- dplyr::mutate_if(x, is.character, function(v) {
    replace(v, which(nchar(v) == 0), NA_character_)
  })
}
