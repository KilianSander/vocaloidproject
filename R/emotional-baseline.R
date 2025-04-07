#' Individual emotional baseline
#'
#' This function creates a page examining the emotional baseline of
#' a participant.
#'
#'
emotional_baseline_page <- function(label = "emotional_baseline",
                                    dict = vocaloidproject::vocaloidproject_dict,
                                    default_lang = "de_f") {
  stopifnot(psyquest:::is.scalar.character(label),
            is.character(default_lang))
  ui <-
    shiny::div(

      GAR::make_ui_radiobutton_multi_NAFC(
        label = label,
        items = purrr::map_vec(paste0("EMOBASE_ITEM", 1:6), psychTestR::i18n),
        choices = 1:5,
        labels = purrr::map(paste0("EMOBASE_CHOICE", 1:5), psychTestR::i18n),
        trigger_button_text = psychTestR::i18n("NEXT"),
        hide = FALSE,
        id = "response_ui"
      )
    )

  psychTestR::new_timeline(
    psychTestR::page(
      ui = ui,
      label = label,
      final = FALSE
    ),
    dict = dict,
    default_lang = default_lang
  )
}

# polarity = "bipolar",
# items = purrr::map_vec(paste0("EMOBASE_ITEM", 1:6), psychTestR::i18n),
# choices = 1:5,
# labels = purrr::map(paste0("EMOBASE_CHOICE", 1:5), psychTestR::i18n),
# instruction = psychTestR::i18n("EMOBASE_PROMPT"),
# header = "double",
# trigger_button_text = psychTestR::i18n("NEXT"),
# allow_na = FALSE,
# failed_validation_message = psychTestR::i18n(""),
# save_answer = TRUE,
# hide_response_ui = FALSE,
# random_order = FALSE
