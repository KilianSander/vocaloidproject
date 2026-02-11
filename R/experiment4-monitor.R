#' Monitor App for Experiment 4
#'
#' `r lifecycle::badge("experimental")`
#' This function calls a simple monitor app for Experiment 4 as defined by
#' [experiment4()].
#' The function assumes that it is running on the same shiny server as
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
#' @param data_pw (character scalar) `r lifecycle::badge("experimental")`
#' set a password for data access.
#' Caution: This is a very simple implementation.
#'
experiment4_monitor <- function(battery_folder_1 = "",
                                battery_folder_2 = "",
                                battery_folder_3 = "",
                                sosci_data_url = NULL,
                                data_pw = "supersecretpassword") {
  # argument checks -----
  stopifnot(is.scalar.character(battery_folder_1),
            is.scalar.character(battery_folder_2),
            is.scalar.character(battery_folder_3),
            is.scalar.character(sosci_data_url) | is.null(sosci_data_url),
            # is.scalar.character(starting_date) | is.null(starting_date),
            is.scalar.character(data_pw))
  # pre processing -----
  path1 <- file.path("..", battery_folder_1, "output", "results")
  path2 <- file.path("..", battery_folder_2, "output", "results")
  path3 <- file.path("..", battery_folder_3, "output", "results")
  # if (!is.null(starting_date)) {
  #   starting_date <- as.POSIXct(starting_date)
  # }
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
      shiny::selectInput(
        inputId = "selected_languages",
        label = "Include Languages",
        choices = c("German" = "de_f", "Japanese" = "ja"),
        multiple = TRUE,
        selected = c("de_f", "ja")
      ),
      shiny::selectInput(
        inputId = "selected_designs",
        label = "Include Designs",
        choices = letters[1:4] %>% stats::setNames(., paste0("Design ", .)),
        multiple = TRUE,
        selected = letters[1:4]
      ),
      shiny::selectInput(
        inputId = "selected_sessions",
        label = "Include Sessions",
        choices = stats::setNames(1:3, paste0("Session ", 1:3)),
        multiple = TRUE,
        selected = 1:3
      ),
      shiny::dateInput(
        inputId = "starting_date",
        label = "Use data starting from",
        value = "2026-02-01",
        min = "2026-01-01"
      ),
      shiny::tabsetPanel(
        id = "download_panel",
        type = "hidden",
        selected = "NoDownload",
        shiny::tabPanel(
          "Downloads",
          shiny::downloadButton(
            "downloadData",
            "Download complete data set"
          ),
          shiny::downloadButton(
            "downloadDataFiltered",
            "Download filtered data set"
          )
        ),
        shiny::tabPanel("NoDownload")
      )
    ),
    bslib::nav_panel(
      title = "Summary",
      bslib::layout_columns(
        # shiny::verbatimTextOutput("testo"),
        ### summary goes here ---------------------------
        bslib::card(
          bslib::card_title("Summary data set in total"),
          DT::DTOutput("data_all_summary")
        ),
        bslib::card(
          bslib::card_title("Summary filtered data"),
          DT::DTOutput("data_filtered_summary")
        )
      ),
      bslib::layout_columns(
        bslib::card(
          bslib::card_title("Filtered Data"),
          DT::DTOutput("data_filtered")
        )
      )
    ),
    bslib::nav_panel(
      title = "Raw data psychTestR Session 1",
      DT::dataTableOutput("psychtestr_session1")
    ),
    bslib::nav_panel(
      title = "Raw data psychTestR Session 2",
      DT::dataTableOutput("psychtestr_session2")
    ),
    bslib::nav_panel(
      title = "Raw data psychTestR Session 3",
      DT::dataTableOutput("psychtestr_session3")
    ),
    bslib::nav_panel(
      title = "Raw data SoSci Survey All Sessions",
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
          shiny::updateTabsetPanel(
            inputId = "download_panel", selected = "NoDownload"
          )
        } else {
          message("correct password\n")
          shiny::updateTabsetPanel(
            inputId = "download_panel", selected = "Downloads"
          )
        }
      }
    )

    ## data -----
    date_time <- shiny::eventReactive(
      input$get_data, {
        stringr::str_replace_all(
          as.character(Sys.time()), c(":" = "-", " " = "_")
        )
      }
    )
    dat_list <- shiny::eventReactive(
      input$get_data, {
        if (input$password == data_pw) {
          psychtestr_session1 <- read_experiment4_data(path1)
          psychtestr_session2 <- read_experiment4_data(path2)
          psychtestr_session3 <- read_experiment4_data(path3)
          if (!is.null(sosci_data_url)) {
            sosci_data <-
              sosci_api_import(sosci_data_url) %>%
              dplyr::filter(session %in% c(13, 23, 33)) %>%
              dplyr::mutate(
                session = as.numeric(stringr::str_sub(session, 1, 1))
              ) %>%
              dplyr::select(!dplyr::matches(remove_times)) %>%
              dplyr::select(!dplyr::matches(c("SL0[1-9]_[0-9]{2}"))) %>%
              dplyr::select(
                !dplyr::matches(
                  "(jpop|kpop|vocaloid|hatsunemiku)_(liking|frequency)"
                )
              ) %>%
              dplyr::rename_with(
                .fn = function(x) {
                  stim_times[x]
                },
                .cols = dplyr::all_of(names(stim_times))
              ) %>%
              dplyr::rename_with(
                .fn = function(x) {
                  question_times[x]
                },
                .cols = dplyr::all_of(names(question_times))
              ) %>%
              dplyr::rename_with(
                .cols = dplyr::matches("[AV][0-9]{3}"),
                .fn = function(x) {
                  paste0(
                    c(A = "arousal", V = "valence")[stringr::str_sub(x, 1, 1)],
                    "_", stringr::str_sub(x, 2, 4)
                  )
                }
              ) %>%
              dplyr::rename_with(
                .cols = dplyr::matches("SA05_0[1-8]"),
                .fn = function(x) {
                  x <- stringr::str_sub(x, 7, 7) %>% as.numeric()
                  style <- dplyr::case_when(
                    x < 3 ~ "kpop",
                    x < 5 ~ "jpop",
                    x < 7 ~ "vocaloidmusic",
                    x > 6 ~ "hatsunemiku"
                  )
                  paste0(
                    style, "_", ifelse(x %% 2, "frequency", "liking")
                  )
                }
              ) %>%
              dplyr::rename_with(
                .cols = dplyr::matches("[EL][0-9]{3}_[0-9]{2}"),
                .fn = function(x) {
                  el <- stringr::str_sub(x, 1, 1)
                  stim <- stringr::str_sub(x, 2, 4)
                  item <- stringr::str_sub(x, 7, 7) %>% as.numeric()
                  paste0(
                    purrr::map2_chr(
                      el, item,
                      function(EL, ITEM) {
                        list(
                          E = c(
                            "anger", "melancholy", "rebelliousness",
                            "peacefulness", "inlove", "joy",
                            "despair", "playfulness", "emptiness"
                          ),
                          L = c("intensity", "liking", "familiarity")
                        )[[EL]][[ITEM]]
                      }
                    ),
                    "_", stim
                  )
                }
              )

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

    # Merge data ------------
    data_all_sessions <- shiny::reactive({
      req(dat_list())
      tmp <-
        dat_list()[paste0("psychtestr_session", 1:3)] %>%
        purrr::list_rbind() %>%
        dplyr::rename(
          psychtestr_complete = complete
        )
      if (!is.null(sosci_data_url)) {
        tmp <-
          tmp %>%
          dplyr::left_join(
            y = dat_list()[["sosci_data"]] %>%
              dplyr::select(!c(time_started, FINISHED, Q_VIEWER, TIME_SUM)),
            by = dplyr::join_by(
              p_id == REF,
              exp_session == session,
              exp_design == design,
              language == language
            )
          )
      }
      tmp
    })
    output$data_all_summary <- DT::renderDT({
      req(data_all_sessions())
      data_all_sessions() %>% summarise_data()
    })

    # Filtered data ----------------
    data_filtered <- shiny::reactive({
      req(data_all_sessions())
      data_all_sessions() %>%
        filter_languages(languages = input[["selected_languages"]]) %>%
        filter_designs(designs = input[["selected_designs"]]) %>%
        filter_sessions(sessions = input[["selected_sessions"]]) %>%
        filter_date(date = input[["starting_date"]])
    })
    output$data_filtered <- DT::renderDT({
      data_filtered()
    })
    output$data_filtered_summary <- DT::renderDT({
      req(data_filtered())
      data_filtered() %>% summarise_data()
    })

    # output$testo <- shiny::renderPrint({
    #   req(dat_list())
    #   str(dat_list())
    # })

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
          date_time(),
          ".csv"
        )
      },
      content = function(file) {
        req(data_all_sessions())
        write.csv(data_all_sessions(), file = file, row.names = F)
      }
    )
    output$downloadDataFiltered <- shiny::downloadHandler(
      filename = function() {
        paste0(
          "vocaloid-exp4-data-",
          date_time(),
          "-FILTERED.csv"
        )
      },
      content = function(file) {
        req(data_filtered())
        write.csv(data_filtered(), file = file, row.names = F)
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

stim_times <-
  stats::setNames(
    nm = paste0(
      "TIME",
      sprintf(
        "%03i",
        (12:131) * 2
      )
    ),
    object = paste0(
      "time_stimulus",
      rep(1:4, times = 30),
      sprintf(
        "%02i",
        rep(1:30, each = 4)
      )
    )
  )

question_times <-
  stats::setNames(
    object = paste0(
      "time_",
      c(
        paste0(
          "IAPS", c(5836, 5626, 1720, 7039, 9530, 1441)
        ),
        paste0(
          "emotion_rating_",
          c("info", "melancholy", "peacefulness", "despair", "joy")
        ),
        "roSAS", "genre_info", "genre_rating",
        paste0(
          "example_",
          c("pre_info", "rating", "post_info")
        ),
        "stimulus_info",
        paste0(
          "block", 1:4, "page"
        )
      )
    ),
    nm = c(
      paste0(
        "TIME",
        sprintf(
          "%03i", c(
            3:8, 9:13, 15, 17:18, 19:21, 22, 264:267
          )
        )
      )
    )
  )

remove_times <-
  paste0(
    "TIME",
    sprintf(
      "%03i",
      c(
        seq(from = 25, to = 263, by = 2), # stimuli check time pages
        268:274, # Block 5 & 6 (old implementation, see `international1()`),
        276      # redirects for happy clickers, screen outs, and completes
      )
    )
  )

filter_date <- function(data,
                        date = "2026-01-01",
                        date_column = "time_started") {
  date <- as.POSIXct(date)
  data %>%
    dplyr::filter(.data[[date_column]] >= date)
}
filter_sessions <- function(data,
                            sessions = 1:3,
                            session_column = "exp_session") {
  data %>%
    dplyr::filter(.data[[session_column]] %in% sessions)
}
filter_designs <- function(data,
                           designs = letters[1:4],
                           design_column = "exp_design") {
  data %>%
    dplyr::filter(.data[[design_column]] %in% designs)
}
filter_languages <- function(data,
                             languages = c("de_f", "ja"),
                             language_column = "language") {
  data %>%
    dplyr::filter(.data[[language_column]] %in% languages)
}
summarise_data <- function(data) {
  data %>%
    dplyr::summarise(
      .by = c(language, exp_design, exp_session),
      N = dplyr::n(),
      `probably complete and valid` = sum(completed)
    )
}
