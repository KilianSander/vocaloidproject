voice_rating_monitor <- function(battery_folder_name = "voice-rating",
                                 offline_results_dir = "data-raw") {
  require(shiny)
  require(tidyverse)
  # thanks to Klaus
  on_server <- grepl("shiny-server", getwd())
  if (on_server) {
    results_dir <- file.path(
      "..", battery_folder_name, "output", "results"
    )
  } else {
    results_dir <- offline_results_dir
  }

  ui <- fluidPage(
    #
  )

  server <- function(input, output, session) {
    #
  }
}
