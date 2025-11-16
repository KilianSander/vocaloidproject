#' Monitor App for International Experiment 1
#'
#' `r lifecycle::badge("experimental")`
#' This function calls a simple monitor app for International Experiment 1 as
#' defined in [international1()].
#' The function assumes that it is running on a shiny server.
#'
#' @param battery_folder_name (character scalar) This is the name of the folder
#' on your shiny server where the `app.R` containing `international1(...)`
#' lives.
#'
#' @param battery_folder_name2 (`NULL` or character scalar) When there are two
#' instances of the `international1` battery (e.g., one for each of the two
#' sessions), specify the second instance here.
#' If there is only one, set `battery_folder_name2` to `NULL`.
#'
#' @param external_data (`NULL` or character scalar) if non-`NULL`, a URL to
#' load extra data from.
#'
#' @param external_data_type (character scalar) specifies the import script for
#' data to load from `external_data`.
#' Currently, the only option is `sosci` for the SoSci Survey API for data
#' retrieval as a csv for R.
#'
international1_monitor <- function(battery_folder_name = "international1-1",
                                   battery_folder_name2 = NULL,
                                   external_data = NULL,
                                   external_data_type = "sosci") {

  stopifnot(
    is.scalar.character(battery_folder_name),
    is.null(battery_folder_name2) | is.scalar.character(battery_folder_name2),
    is.null(external_data) | is.scalar.character(external_data)
  )
  if (!is.null(external_data)) {
    external_data_type <- match.arg(external_data_type)
  }
  #
  if (!is.null(external_data)) {
    if (external_data_type == "sosci") {
      extra_data <-
        sosci_api_import(external_data)
    }
  }
  # shiny::shinyApp(ui = ui, server = server)
}
