#' @include internal.R
NULL

#' Add a validator for maximum number of characters to a Handsontable widget.
#'
#' Add a validator to that specifies the maximum number of characters
#' permitted in a column.
#'
#' @param hot [rhandsontable::rhandsontable()] object.
#'
#' @param col vector of column names or indices.
#'
#' @param nchar `numeric` character limit.
#'
#' @param allowInvalid `logical` specifying whether invalid data will be
#'  accepted. Invalid data cells will be color red.
#'
#' @seealso [rhandsontable::hot_col()].
#'
#' @examples
#' # load package
#' library(rhandsontable)
#'
#' # create rhandsontable widget
#' h <- rhandsontable(iris)
#'
#' # add validator
#' h <- add_hot_col_nchar_validator(col = "Species", nchar = 20)
#'
#' @noRd
add_hot_col_nchar_validator <- function(hot, col, nchar, allowInvalid = TRUE) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(col, c("character", "numeric")),
    assertthat::is.count(nchar),
    assertthat::noNA(nchar),
    assertthat::is.flag(allowInvalid),
    assertthat::noNA(allowInvalid)
  )

  # add validator to widget
  rhandsontable::hot_col(
    hot = hot,
    col = col,
    allowInvalid = allowInvalid,
    validator = paste0("
     function (value, callback) {
        setTimeout(function() {
          callback(value.toString().length <= ", nchar, ");
        }, 1000)
      }
    ")
   )
}
