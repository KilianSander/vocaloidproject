info_page <- function(dict = vocaloidproject::vocaloidproject_dict,
                      default_lang = "de_f") {
  psychTestR::new_timeline(
    psychTestR::one_button_page(
      body = psychTestR::i18n("info_text"),
      button_text = psychTestR::i18n("CONTINUE")
    ),
    dict = dict,
    default_lang = default_lang
  )
}
