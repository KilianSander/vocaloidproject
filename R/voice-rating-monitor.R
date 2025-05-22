voice_rating_monitor <- function(battery_folder_name = "voice-rating",
                                 offline_results_dir = "data-raw") {
  require(shiny)

  require(DT)
  require(dplyr)
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

  ui <- fluidPage(
    titlePanel("Voice Rating Monitor"),
    # bslib::layout_columns()
    DT::dataTableOutput("data_raw")
  )

  server <- function(input, output, session) {
    check_data <-
      reactiveFileReader(
        1000, session, results_dir, setup_voice_rating_workspace
      )

    output$data_raw <- DT::renderDataTable({
      check_data()
      master
    }, rownames = FALSE)

    output$download_all_data_csv <-
      downloadHandler(
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

  shinyApp(ui = ui, server = server)
}
