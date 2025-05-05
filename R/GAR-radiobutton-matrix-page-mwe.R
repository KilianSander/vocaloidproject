emo <- function(default_lang = "de_f") {
  psychTestR::new_timeline(
    GAR::audio_radiobutton_matrix_page(
      label = "emotional_baseline",
      url = "",
      polarity = "unipolar",
      items = #sapply(
        paste0("EMOBASE_ITEM", 1:6),
      #   psychTestR::i18n,
      #   simplify = TRUE, USE.NAMES = TRUE
      # ),
      choices = as.character(1:5),
      instruction = psychTestR::i18n("EMOBASE_PROMPT"),
      labels = purrr::map_vec(
        paste0("EMOBASE_CHOICE", 1:5),
        psychTestR::i18n
      ),
      anchors = FALSE,
      header = "double",
      sublabel_type = "directed",
      reduce_labels = TRUE,
      trigger_button_text = psychTestR::i18n("CONTINUE"),
      allow_na = FALSE,
      failed_validation_message = psychTestR::i18n("ANSWER_NEEDED"),
      save_answer = TRUE,
      hide_response_ui = FALSE,
      random_order = FALSE,
      response_ui_id = "response_ui"
    ),
    dict = vocaloidproject::vocaloidproject_dict,
    default_lang = default_lang
  )
}
psychTestR::make_test(
  elts = psychTestR::join(
    psychTestR::one_button_page(
      body = shiny::tags$h2("hello there"),
      button_text = "continue"
    ),
    emo(default_lang = "de_f"),
    psychTestR::final_page("bye!")
  ),
  opt = psychTestR::test_options(
    title = "Test GAR::radiobutton_matrix_page()",
    admin_password = "matrix",
    languages = c("de_f", "ja")
  )
)
