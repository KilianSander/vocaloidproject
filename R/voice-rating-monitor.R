#' Voice Rating Monitor App
#'
#' This functions calls the monitor app for the voice rating battery (see
#' [voice_rating_battery()]).
#'
#' @param battery_folder_name (character scalar) When using the app on a shiny
#' server that hosts the voice rating battery, this is the name of the folder
#' where the `app.R` containing the voice rating battery lives.
#' @param offline_results_dir When you run the monitor app locally
#' (e.g., in RStudio) you need specify a directory where the `psychTestR`
#' results of [voice_rating_battery()] are stored.
#' When using [voice_rating_battery()] locally, this is `output/results` in
#' the current working directory.
#'
#' @export
voice_rating_monitor <- function(battery_folder_name = "voice-rating",
                                 offline_results_dir = "data-raw") {
  stopifnot(is.scalar.character(battery_folder_name),
            is.scalar.character(offline_results_dir))
  # require(shiny)
  #
  # require(DT)
  # require(dplyr)
  # thanks to Klaus
  on_server <- grepl("shiny-server", getwd())
  if (on_server) {
    results_dir <- file.path(
      "..", battery_folder_name, "output", "results"
    )
  } else {
    results_dir <- offline_results_dir
  }

  setup_voice_rating_workspace(results = results_dir, reload = FALSE)

  ui <- shiny::fluidPage(
    shiny::titlePanel("Voice Rating Monitor"),
    shiny::tabsetPanel(
      shiny::tabPanel(
        title = "Summary",
        shiny::fluidRow(
          shiny::column(
            width = 4,
            DT::dataTableOutput("summary_tbl")
          ),
          shiny::column(
            width = 8,
            plotly::plotlyOutput("gender_plot")
          )
        )
      ),
      shiny::tabPanel(
        title = "Data",
        DT::dataTableOutput("data_raw")
      ),
      shiny::tabPanel(
        title = "Download",
        shiny::downloadButton(
          "download_all_data_csv",
          label = "Download Data Set"
        )
      )
    )
    # bslib::layout_columns()
  )

  server <- function(input, output, session) {
    check_data <-
      shiny::reactiveFileReader(
        1000, session, results_dir, setup_voice_rating_workspace
      )

    output$data_raw <- DT::renderDataTable({
      check_data()
      master
    }, rownames = FALSE)

    output$gender_plot <- plotly::renderPlotly({
      check_data()
      plot <-
        master %>%
        ggplot2::ggplot() +
        ggplot2::geom_bar(
          ggplot2::aes(
            x = Gender
          )
        )
      plotly::ggplotly(plot)
    })

    output$summary_tbl <- DT::renderDataTable({
      check_data()
      long <-
        master %>%
        tidyr::pivot_longer(
          cols = tidyr::starts_with("voice_rating_"),
          names_to = "stimulus",
          values_to = "voice_rating",
          names_pattern = "voice_rating_(.*)"
        ) %>%
        tidyr::drop_na(voice_rating)
      data.frame(
        Participants = length(unique(long$p_id)),
        Ratings = nrow(long)
      )
    }, rownames = FALSE, options = list(dom = 't'))

    output$download_all_data_csv <-
      shiny::downloadHandler(
        filename = paste0(
          "voice-rating-data-",
          Sys.time() |>
            as.character() |>
            stringr::str_replace_all(c(":" = "-", " " = "_")),
          ".csv"
        ),
        content = function(file) {
          write.csv(
            master,
            file,
            quote = TRUE,
            fileEncoding = "utf-8"
          )
        }
      )
  }

  shiny::shinyApp(ui = ui, server = server)
}
