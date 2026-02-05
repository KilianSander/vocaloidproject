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
#' @param sosci_data_url (`NULL` or character scalar) if non-`NULL`, a URL to
#' load data from (provided by SoSci Survey).
#'
#' @param starting_date (`NULL` or character scalar) To get data from a
#' specific point in time onwards, specify a date in the format `YYYY-MM-DD`
#' (e.g., to exclude piloting data).
#' Set to `NULL` to get all data.
#'
#' @param data_pw (character scalar) `r lifecycle::badge("experimental")`
#' set a password for data access.
#' Caution: This is a very simple implementation.
#'
experiment4_monitor <- function(battery_folder_1 = "",
                                battery_folder_2 = "",
                                battery_folder_3 = "",
                                sosci_data_url = NULL,
                                starting_date = "2026-01-28",
                                data_pw = "supersecretpassword") {
  # argument checks -----
  stopifnot(is.scalar.character(battery_folder_1),
            is.scalar.character(battery_folder_2),
            is.scalar.character(battery_folder_3),
            is.scalar.character(sosci_data_url) | is.null(sosci_data_url),
            is.scalar.character(starting_date) | is.null(starting_date),
            is.scalar.character(data_pw))
  # pre processing -----
  path1 <- file.path("..", battery_folder_1, "output", "results")
  path2 <- file.path("..", battery_folder_2, "output", "results")
  path3 <- file.path("..", battery_folder_3, "output", "results")
  if (!is.null(starting_date)) {
    starting_date <- as.POSIXct(starting_date)
  }
  # ui ---------------------
  ui <- bslib::page_navbar(
    title = "Vocaloid Project Experiment 4 Data Monitor",
    sidebar = bslib::sidebar(
      shiny::passwordInput(
        inputId = "password",
        label = "Password:"
      ),
      shiny::actionButton(
        "get_data",
        "Get Data From Server!"
      ),
      shiny::downloadButton(
        "downloadData",
        "Download data to local machine"
      )
    ),
    bslib::nav_panel(
      title = "Summary",
      shiny::verbatimTextOutput("testo")
    ),
    bslib::nav_panel(
      title = "psychTestR Session 1",
      DT::dataTableOutput("psychtestr_session1")
    ),
    bslib::nav_panel(
      title = "psychTestR Session 2",
      DT::dataTableOutput("psychtestr_session2")
    ),
    bslib::nav_panel(
      title = "psychTestR Session 3",
      DT::dataTableOutput("psychtestr_session3")
    ),
    bslib::nav_panel(
      title = "SoSci Survey All Sessions",
      shiny::uiOutput("sosci_all_data")
    ),
    # shiny::h5("Filters"),
    # bslib::layout_columns(
    #   bslib::card(
    #     shiny::selectInput(
    #       inputId = "selected_sessions",
    #       label = "Include Sessions",
    #       choices = stats::setNames(1:3, paste0("Session ", 1:3)),
    #       multiple = TRUE,
    #       selected = 1:3
    #     ),
    #     max_height = 200,
    #     min_height = 100
    #   ),
    #   bslib::card(
    #     shiny::selectInput(
    #       inputId = "selected_designs",
    #       label = "Include Designs",
    #       choices = letters[1:4] %>% stats::setNames(., paste0("Design ", .)),
    #       multiple = TRUE,
    #       selected = letters[1:4]
    #     ),
    #     max_height = 200,
    #     min_height = 100
    #   ),
    #   bslib::card(
    #     shiny::selectInput(
    #       inputId = "selected_languages",
    #       label = "Include Languages",
    #       choices = c("German" = "de", "Japanese" = "ja"),
    #       multiple = TRUE,
    #       selected = c("de", "ja")
    #     ),
    #     max_height = 200,
    #     min_height = 100
    #   ),
    #   max_height = 200,
    #   min_height = 100
    # ),
    # bslib::layout_columns(
    #   shiny::actionButton(
    #     inputId = "apply_filters",
    #     label = "Apply Filters"
    #   )
    # ),
    # bslib::layout_columns(
    #   bslib::card(
    #     bslib::card_header("Selected Sessions"),
    #     shiny::verbatimTextOutput("sel_sessions")
    #   ),
    #   bslib::card(
    #     bslib::card_header("Selected Designs"),
    #     shiny::verbatimTextOutput("sel_designs")
    #   ),
    #   bslib::card(
    #     bslib::card_header("Selected Languages"),
    #     shiny::verbatimTextOutput("sel_languages")
    #   ),
    #   max_height = 100
    # ),
    # bslib::layout_columns(
    #   shiny::verbatimTextOutput("testo")
    # ),
    theme = bslib::bs_theme(
      version = 5,
      preset = "shiny"
    ),
    lang = "en"
  )
  # server --------------
  server <- function(input, output, session) {
    ## wrong password -----
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

    ## data -----
    dat_list <- shiny::eventReactive(
      input$get_data, {
        if (input$password == data_pw) {
          psychtestr_session1 <- read_experiment4_data(path1)
          psychtestr_session2 <- read_experiment4_data(path2)
          psychtestr_session3 <- read_experiment4_data(path3)
          if (!is.null(sosci_data_url)) {
            sosci_data <- sosci_api_import(sosci_data_url)
          }
          ret <- list(
            "psychtestr_session1" = psychtestr_session1,
            "psychtestr_session2" = psychtestr_session2,
            "psychtestr_session3" = psychtestr_session3
          )
          if (!is.null(sosci_data_url)) {
            ret[["sosci_data"]] <- sosci_data
          }
          # if (!is.null(starting_date)) {
          #   ret <-
          #     purrr::map(
          #       ret,
          #       function(x) {
          #         x %>%
          #           dplyr::filter(time_started >= starting_date)
          #       },
          #     )
          # }
          ret
        }
      }
    )

    output$testo <- shiny::renderPrint({
      req(dat_list())
      str(dat_list())
    })

    output$psychtestr_session1 <- DT::renderDataTable({
      req(dat_list())
      dat_list()[["psychtestr_session1"]]
    })
    output$psychtestr_session2 <- DT::renderDataTable({
      req(dat_list())
      dat_list()[["psychtestr_session2"]]
    })
    output$psychtestr_session3 <- DT::renderDataTable({
      req(dat_list())
      dat_list()[["psychtestr_session3"]]
    })
    output$sosci_all_data <- shiny::renderUI({
      if (!is.null(sosci_data_url)) {
        DT::renderDataTable({
          req(dat_list())
          dat_list()[["sosci_data"]]
        })
      } else {
        shiny::HTML("No external data.")
      }
    })
    ## download -----
    output$downloadData <- shiny::downloadHandler(
      filename = function() {
        paste0(
          "vocaloid-exp4-data-",
          stringr::str_replace_all(
            as.character(Sys.time()),
            c(":" = "-", " " = "_")
          ),
          ".csv"
        )
      },
      content = function(file) {
        req(dat_list())
        write.csv(dat_list()[[1]], file = file, row.names = F)
      }
    )

    ## filter selection -----------
    # shiny::observeEvent(
    #   input$apply_filters, {
    #     shiny::validate(
    #       shiny::need(!is.null(input$selected_sessions), "Select at least one session"),
    #       shiny::need(!is.null(input$selected_designs), "Select at least one design"),
    #       shiny::need(!is.null(input$selected_languages), "Select at least one language")
    #     )
    #   }
    # )
    ### current filter text output -----
    # output$sel_sessions <- shiny::renderPrint({
    #   input$apply_filters
    #   shiny::isolate(print(input$selected_sessions))
    # })
    # output$sel_designs <- shiny::renderPrint({
    #   input$apply_filters
    #   shiny::isolate(print(input$selected_designs))
    # })
    # output$sel_languages <- shiny::renderPrint({
    #   input$apply_filters
    #   shiny::isolate(print(input$selected_languages))
    # })
  }
  shiny::shinyApp(ui = ui, server = server)
}

read_experiment4_data <- function(results_dir) {
  results <- purrr::map(
    list.files(path = results_dir, pattern = "*.rds$", full.names = TRUE),
    function(x) {
      readRDS(x) %>% as.list()
    }
  )
  if (length(results) > 0) {
    ret <-
      results %>%
      purrr::map(
        function(x) {
          # browser()
          x <- as.list(x)
          if (!is.null(x$session)) {
            session_data <-
              x$session %>%
              as.data.frame() %>%
              dplyr::select(
                dplyr::all_of(
                  c("p_id", "language", "time_started", "complete")
                )
              )
          }
          if (is.null(x$session) || nrow(session_data) == 0) {
            session_data <- data.frame(p_id = NA)
          }

          exp_session <- x[["results"]][["uses"]]
          exp_design <- x[["results"]][["udes"]]

          session_data$exp_session <- exp_session
          session_data$exp_design <- exp_design
          session_data$consent <-
            ifelse(
              !is.null(x[["results"]][["consent"]]),
              x[["results"]][["consent"]],
              NA_character_
            )

          if (is.null(exp_session) | (!(exp_session %in% 1:3))) {
            person_data <- session_data
          } else {
            emo_baseline <- parse_emotional_baseline(x[["emotional_baseline"]])
            halt <- parse_HALT_selfreport_device(x[["HALT"]])
            ## Session 1 data -----
            if (!is.null(x[["DEG"]])) {
              deg <-
                parse_deg(x[["DEG"]])
            } else {
              deg <-
                data.frame(
                  Gender = NA_character_,
                  Age_months = NA_real_,
                  Age_years = NA_real_
                )
            }
            if (!is.null(x[["demographics"]])) {
              demographics <-
                x[["demographics"]] %>%
                as.data.frame()
            } else {
              demographics <-
                data.frame(first_language = NA_character_)
            }
            ## Session 2 data -----
            asa <- parse_asa(x[["ASA"]])
            if (!is.null(x[["GMS"]])) {
              gms <-
                x[["GMS"]] %>%
                as.data.frame() %>%
                dplyr::rename_with(
                  function(col) {
                    paste0("GMS_", col)
                  }
                )
            } else {
              gms <-
                data.frame(GMS_General = NA_real_)
            }
            ## Session 3 data -----
            bes <- parse_bes(x[["BES"]])

            person_data <-
              dplyr::bind_cols(
                session_data, emo_baseline, halt,
                deg, demographics, asa, gms, bes
              )
          }

          return(person_data)
        }
      ) %>%
      purrr::list_rbind() %>%
      dplyr::arrange(time_started)
  } else {
    ret <- data.frame(
      p_id = character(0L)
    )
  }
  return(ret)
}
