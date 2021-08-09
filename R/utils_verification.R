#' @include internal.R
NULL

#' Has duplicates?
#'
#' Check if a vector has duplicates?
#'
#' @param x `character` `vector` object.
#'
#' @return `logical` value.
#'
#' @noRd
has_unique_values <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(TRUE)
  identical(anyDuplicated(x), 0L)
}

#' Has valid values?
#'
#' Check if a vector has valid values?
#'
#' @param x `character` `vector` object.
#'
#' @return `logical` value.
#'
#' @noRd
has_valid_values <- function(x) {
  isTRUE((length(x) > 0) && assertthat::noNA(x) && all(nchar(x) > 0))
}
