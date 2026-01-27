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
    shiny::h5("Filters"),
    bslib::layout_columns(
      bslib::card(
        shiny::selectInput(
          inputId = "selected_sessions",
          label = "Include Sessions",
          choices = stats::setNames(1:3, paste0("Session ", 1:3)),
          multiple = TRUE,
          selected = 1:3
        ),
        max_height = 200,
        min_height = 100
      ),
      bslib::card(
        shiny::selectInput(
          inputId = "selected_designs",
          label = "Include Designs",
          choices = letters[1:4] %>% stats::setNames(., paste0("Design ", .)),
          multiple = TRUE,
          selected = letters[1:4]
        ),
        max_height = 200,
        min_height = 100
      ),
      bslib::card(
        shiny::selectInput(
          inputId = "selected_languages",
          label = "Include Languages",
          choices = c("German" = "de", "Japanese" = "ja"),
          multiple = TRUE,
          selected = c("de", "ja")
        ),
        max_height = 200,
        min_height = 100
      ),
      max_height = 200,
      min_height = 100
    ),
    bslib::layout_columns(
      shiny::actionButton(
        inputId = "apply_filters",
        label = "Apply Filters"
      ),
      shiny::textOutput("filter_selection"),
      shiny::verbatimTextOutput("test")
    ),
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

    shiny::observeEvent(
      input$apply_filters, {
        shiny::validate(
          shiny::need(!is.null(input$selected_sessions), "Select at least one session"),
          shiny::need(!is.null(input$selected_designs), "Select at least one design"),
          shiny::need(!is.null(input$selected_languages), "Select at least one language")
        )

        output$filter_selection <- shiny::renderText({
          c(
            paste0(
              "Selected sessions: ",
              stringr::str_flatten_comma(input$selected_sessions, last = ", and ")
            ),
            paste0(
              "Selected designs: ",
              stringr::str_flatten_comma(input$selected_designs, last = ", and ")
            ),
            paste0(
              "\nSelected languages: ",
              stringr::str_flatten(input$selected_languages, collapse = " and ")
            )
          )
        }, sep = ";\n")
        output$test <- shiny::renderPrint(input$selected_sessions)
      }
    )
  }
  shiny::shinyApp(ui = ui, server = server)
}
