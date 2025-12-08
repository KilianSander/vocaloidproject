# scoring maps
questionnaires <- c(
  # "ASA",
  "BES"
)

scoring_maps <-
  purrr::map(
    questionnaires,
    function(quest) {
      read.csv(
        file = paste0("data-raw/scoring/", quest, "_scoring.csv")
      ) |>
        dplyr::mutate(
          questionnaire = quest,
          key = sprintf("%s_%04i_PROMPT", quest, item),
          item = paste0("q", item)
        )
    }
  ) |>
  stats::setNames(
    questionnaires
  )

usethis::use_data(
  scoring_maps,
  overwrite = TRUE
)
