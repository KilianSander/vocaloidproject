#' Monitor App for Experiment 4
#'
#' `r lifecycle::badge("experimental")`
#' This function calls a simple monitor app for Experiment 4 as defined by
#' [experiment4()].
#' The function assumed that it is running on the same shiny server as
#' [experiment4()].
#'
#' @param battery_folder_1 (character scalar) This is the name of the folder
#' on your shiny server where the `app.R` containing
#' `experiment4(session_number = 1, ...)`
#' lives (i.e. app for Session 1).
#'
#' @param battery_folder_2 (character scalar) This is the name of the folder
#' on your shiny server where the `app.R` containing
#' `experiment4(session_number = 2, ...)`
#' lives (i.e. app for Session 2).
#'
#' @param battery_folder_3 (character scalar) This is the name of the folder
#' on your shiny server where the `app.R` containing
#' `experiment4(session_number = 3, ...)`
#' lives (i.e. app for Session 3).
#'
#' @param sosci_data (`NULL` or character scalar) if non-`NULL`, a URL to
#' load data from (provided by SoSci Survey).
#'
#' @param data_pw (character scalar) `r lifecycle::badge("experimental")`
#' set a password for data access.
#' Caution: This is a very simple implementation.
#'
experiment4_monitor <- function(battery_folder_1 = "",
                                battery_folder_2 = "",
                                battery_folder_3 = "",
                                sosci_data = NULL,
                                data_pw = "supersecretpassword") {
  stopifnot(is.scalar.character(battery_folder_1),
            is.scalar.character(battery_folder_2),
            is.scalar.character(battery_folder_3),
            is.scalar.character(sosci_data) | is.null(sosci_data),
            is.scalar.character(data_pw))
  ui <- bslib::page_sidebar(
    title = "Vocaloid Project Experiment 4 Data Monitor",
    sidebar = bslib::sidebar(
      shiny::passwordInput(
        inputId = "password",
        label = "Password:"
      ),
      shiny::actionButton(
        "get_data",
        "Get Data From Server!"
      )
    ),
    bslib::layout_columns(),
    theme = bslib::bs_theme(
      version = 5,
      preset = "shiny"
    ),
    lang = "en"
  )
  server <- function(input, output, session) {
    # wrong password -----
    shiny::observeEvent(
      input$get_data, {
        message("button pressed")
        if (input$password != data_pw) {
          message("wrong password\n")
          shiny::showNotification(
            ui = "Wrong password!",
            duration = NULL,
            closeButton = TRUE
          )
        } else {
          message("correct password\n")
        }
      }
    )

    # dat <- shiny::eventReactive()

  }
  shiny::shinyApp(ui = ui, server = server)
}
