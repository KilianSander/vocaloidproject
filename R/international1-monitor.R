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
#' @param title (character scalar) Title for the app.
#'
#' @param data_pw (character scalar) `r lifecycle::badge("experimental")`
#' set a password for data access.
#' Caution: This is a very simple implementation.
#'
international1_monitor <- function(battery_folder_name = "international1-1",
                                   battery_folder_name2 = NULL,
                                   external_data = NULL,
                                   external_data_type = "sosci",
                                   title = "Vocaloid Project Experiment 4 (International 1) Monitor",
                                   data_pw = "supersecretpassword") {

  stopifnot(
    is.scalar.character(battery_folder_name),
    is.null(battery_folder_name2) | is.scalar.character(battery_folder_name2),
    is.null(external_data) | is.scalar.character(external_data)
  )

  sec_folder <- !is.null(battery_folder_name2)
  extra_d <- !is.null(external_data)

  results_dir <- file.path(
    "..", battery_folder_name, "output", "results"
  )

  if (sec_folder) {
    results_dir2 <- file.path(
      "..", battery_folder_name2, "output", "results"
    )
  }

  if (extra_d) {
    external_data_type <- match.arg(external_data_type)
  }

  ui <-
    shiny::fluidPage(
      theme = bslib::bs_theme(
        version = 5,
        preset = "shiny"
      ),
      shiny::titlePanel(title = title),
      ## password and button -----
      bslib::layout_columns(
        shiny::passwordInput(
          inputId = "password",
          label = "Password:"
        ),
        shiny::actionButton(
          "get_data",
          "Get Data!"
        )
      ),
      shiny::tabsetPanel(
        shiny::tabPanel(
          ## Tab sample summary -----
          title = "Sample summary",
          bslib::layout_columns(
            bslib::card(
              bslib::card_header("Session 1 Total Sample"),
              bslib::card_body()
            ),
            bslib::card(
              bslib::card_header("Session 2 Total Sample"),
              bslib::card_body()
            )
          ),
          bslib::layout_columns(
            bslib::card(
              bslib::card_header("Session 1 German Sample"),
              bslib::card_body()
            ),
            bslib::card(
              bslib::card_header("Session 2 German Sample"),
              bslib::card_body()
            )
          ),
          bslib::layout_columns(
            bslib::card(
              bslib::card_header("Session 1 Japanese Sample"),
              bslib::card_body()
            ),
            bslib::card(
              bslib::card_header("Session 2 Japanese Sample"),
              bslib::card_body()
            )
          )
        ),
        ## Tab data -----
        shiny::tabPanel(
          title = "Raw Data",
          DT::DTOutput("data_raw")
        ),
        shiny::tabPanel(
          title = "Complete Cases",
          DT::DTOutput("table")
        ),
        shiny::tabPanel(
          title = "Suspicious Cases",
          shiny::verbatimTextOutput("sus")
        )
      )

    )

  server <- function(input, output, session) {
    # get data -----
    data_list <- shiny::eventReactive(
      input$get_data, {
        if (input$password == data_pw) {
          ## first results dir -----
          data_raw <-
            read_international1_data(results_dir = results_dir)
          message("First directory read")
          ## second results dir -----
          if (sec_folder) {
            data_raw2 <-
              read_international1_data(results_dir = results_dir2)
            message("Second directory read")
            # message("Attempt to merge")
            for (i in base::setdiff(names(data_raw), names(data_raw2))) {
              data_raw2[i] <- NA
            }
            for (i in base::setdiff(names(data_raw2), names(data_raw))) {
              data_raw[i] <- NA
            }
            data_raw <-
              # purrr:list_rbind(
              #   list(
              #     data_raw,
              #     data_raw2
              #   )
              # )
              rbind(
                data_raw, data_raw2
              )
          }

          session1_p_ids <-
            data_raw %>%
            dplyr::filter(exp_session == 1) %>%
            dplyr::pull("p_id")

          suspicious_cases <-
            data_raw %>%
            dplyr::filter(exp_session == 2) %>%
            dplyr::pull("p_id") %>%
            base::setdiff(y = session1_p_ids)

          ## extra data -----
          if (extra_d) {
            if (external_data_type == "sosci") {
              # get data from sosci
              extra_data <-
                sosci_api_import(external_data) %>%
                dplyr::filter(!is.na(completed)) %>%
                dplyr::select(!dplyr::starts_with("EMOBASE_ITEM"))
              message("Extra data read")
              suspicious_cases <- c(
                suspicious_cases,
                dplyr::pull(
                  dplyr::filter(extra_data, !(REF %in% session1_p_ids)),
                  var = "REF"
                )
              )
              # join with data from psychTestR
              data_raw <-
                dplyr::full_join(
                  data_raw,
                  extra_data,
                  by = dplyr::join_by(
                    p_id == REF,
                    exp_session == session
                  )
                )
            }
          }

          data_pre <-
            data_raw %>%
            dplyr::filter(!(p_id %in% suspicious_cases))

          ret <- list(
            "data_raw" = data_raw,
            "data_pre" = data_pre,
            "suspicious_cases" = suspicious_cases
          )
          if (extra_d) {
            ret[["extra_data"]] <- extra_data
          }
          return(ret)
        }
      }
    )

    # wrong password -----
    shiny::observeEvent(
      input$get_data, {
        message("button pressed\n")
        if (input$password != data_pw) {
          shiny::showNotification(
            ui = "Wrong password!",
            duration = NULL,
            closeButton = TRUE
          )
        }
      }
    )

    # sub-samples -----
    data_de <- reactive({
      req(data_list())
      data_list()$data_pre %>%
        dplyr::filter(language == "de_f")
    })

    data_ja <- reactive({
      req(data_list())
      dat_list()$data_pre %>%
        dplyr::filter(language == "ja")
    })

    # display data -----
    output$table <- DT::renderDT({
      req(data_list())
      data_list()$data_pre
    })

    output$sus <- shiny::renderPrint({
      req(data_list())
      data_list()$suspicious_cases
    })

    output$data_raw <- DT::renderDT({
      req(data_list())
      data_list()$data_raw
    })

    # end of server code -----
  }

  shiny::shinyApp(ui = ui, server = server)
}


read_international1_data <- function(results_dir) {
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

          if (is.null(exp_session) | (!(exp_session %in% 1:2))) {
            person_data <- session_data
          } else {
            emo_baseline <- parse_emotional_baseline(x[["emotional_baseline"]])
            halt <- parse_HALT_selfreport_device(x[["HALT"]])
            ## Session 1 data -----
            if (exp_session == 1) {
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

              person_data <-
                dplyr::bind_cols(
                  session_data, deg, demographics,
                  emo_baseline, halt
                )
            }
            ## Session 2 data -----
            if (exp_session == 2) {
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

              person_data <-
                dplyr::bind_cols(
                  session_data, gms, emo_baseline, halt
                )
            }
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
