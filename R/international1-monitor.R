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
international1_monitor <- function(battery_folder_name = "international1-1",
                                   battery_folder_name2 = NULL) {
  stopifnot(
    is.scalar.character(battery_folder_name),
    is.null(battery_folder_name2) | is.scalar.character(battery_folder_name2)
  )
  #
  # shiny::shinyApp(ui = ui, server = server)
}
