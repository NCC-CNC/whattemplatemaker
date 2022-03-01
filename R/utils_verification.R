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
#' @noRd
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
#' @noRd
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
#' @noRd
valid_nchar <- function(x, length) {
  assertthat::assert_that(
    assertthat::is.count(length),
    assertthat::noNA(length)
  )
  x <- extract_valid_names(x)
  if (length(x) == 0) return(TRUE)
  isTRUE(max(nchar(x)) <= length)
}

#' Valid Excel Spreadsheet name characters
#'
#' Check that the input contains valid characters for an Excel spreadshhet name
#'
#' @inheritParams valid_nchar
#'
#' @inherit valid_nchar return
#'
#' @noRd
valid_spreadsheet_characters <- function(x) {
  # assert arguments are valid
  assertthat::assert_that(
    is.character(x)
  )

  # define invalid excel characters
  chars <- c("\\", "/", "*", "?", ":", "[", "]")

  # verify if invalid characters found
  r <- vapply(chars, FUN.VALUE = logical(length(x)), grepl, x = x, fixed = TRUE)
  !any(c(r), na.rm = TRUE)
}
