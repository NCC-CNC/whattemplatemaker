#' @include internal.R
NULL

#' Add a renderer for invalid descriptions to a Handsontable widget.
#'
#' Add a renderer to indicate cells that contain invalid descriptions.
#'
#' @inheritParams add_hot_col_invalid_id_renderer
#'
#' @inherit add_hot_col_invalid_id_renderer return
#'
#' @examples
#' # load package
#' library(rhandsontable)
#'
#' # create rhandsontable widget
#' h <- rhandsontable(iris)
#'
#' # add validator
#' h <- add_hot_col_invalid_desc_renderer(col = "Species", nchar = 20)
#'
#' @noRd
add_hot_col_invalid_desc_renderer <- function(hot, col, nchar = 20) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(col, c("character", "numeric")),
    assertthat::is.count(nchar),
    assertthat::noNA(nchar)
  )

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

          /// check if value has acceptable length
          valid = valid && (v.length <= ", nchar, ");

          /// apply cell formatting
          td.style.background = valid ? 'white' : '#d9edf7';

        }
      }
    ")
  )
}
