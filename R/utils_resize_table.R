#' @include internal.R
NULL

#' Resize table
#'
#' Resize a table.
#'
#' @param x `data.frame` containing tabular data.
#'
#' @param nrow `integer` number of new rows.
#'
#' @return `data.frame` object.
#'
#' @noRd
#'
#' @examples
#' # try adding rows
#' resize_table(iris[1:3, ], 4)
#'
#' # try removing rows
#' resize_table(iris[1:3, ], 1)
#'
#" # try doing nothing
#' resize_table(iris[1:3, ], 3)
resize_table <- function(x, nrow = NULL) {
  # assert that arguments are valid
  assertthat::assert_that(
    inherits(x, "data.frame"), nrow(x) >= 0, ncol(x) >= 1)
  if (is.null(nrow)) return(x)
  assertthat::assert_that(
    assertthat::is.count(nrow),
    assertthat::noNA(nrow))
  # resize table
  if (nrow > nrow(x)) {
    ## add rows if needed
    x <- x[c(seq_len(nrow(x)), rep(NA_integer_, nrow - nrow(x))), ,
           drop = FALSE]
    rownames(x) <- NULL
  } else {
    ## remove rows if needed
    x <- x[seq_len(nrow), , drop = FALSE]
  }
  # return result
  x
}
