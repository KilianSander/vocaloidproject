#' First language
#'
first_language_page <- function(label = "first_language",
                                dict = vocaloidproject::vocaloidproject_dict) {
  psychTestR::new_timeline(
    psychTestR::NAFC_page(
      label = label,
      prompt = psychTestR::i18n("TDEG_0008_PROMPT"),
      choices = language_codes,
      labels = purrr::map(names(language_codes), psychTestR::i18n),
      button_style = "width:140px"
    ),
    dict = dict
  )
}


language_codes <-
  c(
    "GERMAN" = "de",
    "JAPANESE" = "ja",
    "ENGLISH" = "en",
    "OTHER_LANGUAGE" = "other"
  )
