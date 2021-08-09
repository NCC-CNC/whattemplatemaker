#' Sever function: download data
#'
#' Set behavior for downloading data.
#'
#' @details
#' This object is designed to be used within [app_server] function.
#' Within the [app_server] function, it should be called like this:
#'
#' ```
#' eval(server_map)
#' ```
#'
#' @noRd
server_download_data <- quote({
  # download data
  output$download_btn <- shiny::downloadHandler(
    filename = function() {
      paste("data-template.xlsx", sep = "")
    },
    content = function(path) {
      out_data <- whatdataio::create_template_workbook(
        site_names = values[["site_data"]][[1]],
        feature_names = values[["feature_data"]][[1]],
        action_names = values[["action_data"]][[1]],
        parameters = parameters)
      openxlsx::saveWorkbook(out_data, path, returnValue = FALSE)
  })
})
