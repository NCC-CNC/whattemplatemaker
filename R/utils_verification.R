#' @include internal.R
NULL

#' Extract valid names
#'
#' Extract valid names in a vector.
#'
#' @param x `character` `vector` object.
#'
#' @return `character` value.
#'
#' @export
extract_valid_names <- function(x) {
  if (length(x) == 0) return(c())
  if (all(is.na(x))) return(c())
  x <- x[!is.na(x)]
  x[which(nchar(x) > 0)]
}

#' Number of valid names
#'
#' Identify number of valid names in a vector.
#'
#' @param x `character` `vector` object.
#'
#' @return `logical` value.
#'
#' @export
n_valid_names <- function(x) {
  length(extract_valid_names(x))
}

#' Valid number of characters
#'
#' Check that the valid names in a vector have an acceptable number of
#' characters.
#'
#' @param x `character` `vector` object.
#'
#' @param length `integer` maximum number of characters permitted.
#'
#' @return `logical` value.
#'
#' @export
valid_nchar <- function(x, length) {
  assertthat::assert_that(
    assertthat::is.count(length),
    assertthat::noNA(length)
  )
  x <- extract_valid_names(x)
  if (length(x) == 0) return(TRUE)
  isTRUE(max(nchar(x)) < length)
}
