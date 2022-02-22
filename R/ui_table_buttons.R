#' @include internal.R
NULL

#' Add row button
#'
#' Create a button for adding a row to a table.
#'
#' @details
#' This button is a customized [shinyFeedback::loadingButton()].
#'
#' @param inputId `character` HTML identifier for object.
#'
#' @return A `shiny.tag` object.
#'
#' @noRd
add_row_button <- function(inputId) {
  shinyBS::tipify(
    title = "Insert row",
    shinyFeedback::loadingButton(
      inputId,
      class = "btn btn-sm btn-default fa fa-lg fa-solid fa-plus",
      loadingClass = "btn btn-default btn-sm fa fa-lg",
      style = "font-weight: 800;",
      label = "",
      loadingLabel = ""
    )
  )
}

#' Remove row button
#'
#' Create a button for removing a row from a table.
#'
#' @details
#' This button is a customized [shinyFeedback::loadingButton()].
#'
#' @param inputId `character` HTML identifier for object.
#'
#' @return A `shiny.tag` object.
#'
#' @noRd
remove_row_button <- function(inputId) {
  shinyBS::tipify(
    title = "Remove row",
    shinyFeedback::loadingButton(
      inputId,
      class = "btn btn-sm btn-default fa fa-lg fa-solid fa-minus",
      loadingClass = "btn btn-sm btn-default fa fa-lg",
      style = "font-weight: 800;",
      label = "",
      loadingLabel = ""
    )
  )
}
