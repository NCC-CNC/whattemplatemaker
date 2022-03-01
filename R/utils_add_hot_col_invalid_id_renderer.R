#' @include internal.R
NULL

#' Add a renderer for invalid identifiers to a Handsontable widget.
#'
#' Add a renderer to indicate cells that contain invalid identifier values.
#'
#' @param hot [rhandsontable::rhandsontable()] object.
#'
#' @param col vector of column names or indices.
#'
#' @param nchar `numeric` character limit. Defaults to 1000.
#'
#' @param illegal_characters `character` vector of characters that are
#'  not permitted.
#'  Defaults to a vector containing the following characters: `"\"`, "`/"`,
#'  "`*"`, "`?"`, "`:"`, "`["`, and "`]"`.
#'
#' @return  An updated [rhandsontable::rhandsontable()] object.
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
#' h <- add_hot_col_invalid_id_renderer(col = "Species", nchar = 20)
#'
#' @noRd
add_hot_col_invalid_id_renderer <- function(hot, col, nchar = 20,
                                             illegal_characters = c(
                                                "\\", "/", "*", "?", ":", "[",
                                                "]"
                                              )) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(col, c("character", "numeric")),
    assertthat::is.count(nchar),
    assertthat::noNA(nchar),
    is.character(illegal_characters),
    assertthat::noNA(illegal_characters)
  )

  # doubly escape the \ character
  illegal_characters <- gsub("\\", "\\\\", illegal_characters, fixed = TRUE)

  # define JS code to express illegal characters
  char_js <- paste(paste0("'", illegal_characters, "'"), collapse = ", ")

  # add renderer to widget
  rhandsontable::hot_col(
    hot = hot,
    col = col,
    renderer = paste0("
      function(instance, td, row, col, prop, value, cellProperties) {
        // apply the renderer
        Handsontable.renderers.TextRenderer.apply(this, arguments);

        if (instance.params) {
          // initialization
          let v = Handsontable.helper.stringify(value);
          let valid = true;
          let i = [", char_js, "];

          /// check if value has acceptable length
          valid = valid && (v.length <= ", nchar, ");

          /// check if value has illegal characters
          valid = valid && !(i.some(x => v.includes(x)));

          /// check if value is duplicated (if value is not blank)
          if (v.length > 0) {
            console.log(1);
            let n =
              instance
              .getDataAtCol(col)
              .map(x => Handsontable.helper.stringify(x) == v)
              .reduce((psum, x) => psum + x, 0)
            valid = valid && (n <= 1);
          }

          /// apply cell formatting
          td.style.background = valid ? 'white' : '#d9edf7';

        }
      }
    ")
  )
}
