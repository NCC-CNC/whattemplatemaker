#' @include internal.R
NULL

#' Validate data
#'
#' Validate data input by a user.
#'
#' @param x `data.frame` Table object.
#'
#' @return
#' A `list` object containing a series of `list` objects that contain
#' details for various validation checks. Each of the nested `list`
#' objects contains the following elements:
#'
#' \describe{
#' \item{id}{A `character` value containing a unique identifier for the check.}
#' \item{title}{A `character` value containing a title for the check.}
#' \item{details}{A `character` value containing an explanation of the check.}
#' \item{success}{A `logical` value indicating if the check was passed or not.}
#' }
#'
#' @export
validate_data <- function(x) {
  # assert valid argument
  assertthat::assert_that(
    inherits(x, "data.frame"),
    assertthat::has_name(x, "id"),
    assertthat::has_name(x, "description")
  )

  # extract values
  curr_id <- extract_valid_names(x$id)
  curr_desc <- extract_valid_names(x$description)

  # initialize
  output <- list(

    # check rows
    ## rows must have both id and description
    list(
      id = "row_valid",
      title = "Oops",
      details = paste(
        "Each row must have values in both the \"id\" and",
        "\"description\" columns."
      ),
      success = isTRUE(all(is.na(x$id) == is.na(x$description)))
    ),

    ## at least one id or description
    list(
      id = "row_min",
      title = "Oops",
      details = paste(
        "Please provide details for at least one row in the table."
      ),
      success = isTRUE(length(curr_id) >= 1) || isTRUE(length(curr_desc) >= 1)
    ),

    # check id column
    ## unique values
    list(
      id = "id_unique",
      title = "Oops",
      details = "Each value in the \"id\" column must be unique.",
      success = isTRUE(anyDuplicated(curr_id) <= 1)
    ),

    ## character limit
    list(
      id = "id_nchar",
      title = "Oops",
      details = paste0(
        "Each value in the \"id\" column must be shorter than ",
        whattemplatemaker::get_golem_config("maximum_id_length"),
        " characters in length."
      ),
      success = valid_nchar(
        x$id,
        whattemplatemaker::get_golem_config("maximum_id_length")
      )
    ),

    # check description column
    ## character limit
    list(
      id = "desc_nchar",
      title = "Oops",
      details = paste0(
        "Each value in the \"description\" column must be shorter than ",
        whattemplatemaker::get_golem_config("maximum_description_length"),
        " characters in length."
      ),
      success = valid_nchar(
        x$description,
        whattemplatemaker::get_golem_config("maximum_description_length")
      )
    )
  )
}
