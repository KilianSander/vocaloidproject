#' First language
#'
#' @param label (character scalar)
#' @inheritParams last_page_redirect
#'
#' @export
first_language_page <- function(label = "first_language",
                                dict = vocaloidproject::vocaloidproject_dict,
                                default_lang = "de_f") {
  stopifnot(psyquest:::is.scalar.character(label))
  psychTestR::new_timeline(
    psychTestR::NAFC_page(
      label = label,
      prompt = psychTestR::i18n("TDEG_0008_PROMPT"),
      choices = language_codes,
      labels = purrr::map(names(language_codes), psychTestR::i18n),
      button_style = "width:140px"
    ),
    dict = dict,
    default_lang = default_lang
  )
}

#' Language codes
#'
#' A named vector with two-letter language codes.
#' Names are the corresponding dictionary keys.
#'
language_codes <-
  c(
    "GERMAN" = "de",
    "JAPANESE" = "ja",
    "ENGLISH" = "en",
    "OTHER_LANGUAGE" = "other"
  )
