#' First language
#'
#' This function creates a page to ask for the first language.
#' For this research project, the choices are German (`de`), Japanese (`ja`),
#' English (`en`), and other language (`other`).
#'
#' @param label (scalar character) Label for the current page.
#' @inheritParams last_page_redirect
#'
#' @export
first_language_page <- function(label = "first_language",
                                dict = vocaloidproject::vocaloidproject_dict,
                                default_lang = "de_f") {
  stopifnot(is.scalar.character(label),
            is.character(default_lang))
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

#' Japanese skills
#'
#' This function creates a page to ask for Japanese skills.
#'
#' @inheritParams first_language_page
#'
#' @export
japanese_skills_page <- function(label = "japanese_skills",
                                 dict = vocaloidproject::vocaloidproject_dict,
                                 default_lang = "de_f") {
  stopifnot(is.scalar.character(label),
            is.character(default_lang))
  psychTestR::new_timeline(
    psychTestR::NAFC_page(
      label = label,
      prompt = psychTestR::i18n("JAPANESE_SKILLS_PROMPT"),
      choices = as.character(1:4),
      labels = purrr::map(
        paste0("JAPANESE_SKILLS_CHOICE", 1:4),
        psychTestR::i18n
      ),
      button_style = "width:140px"
    ),
    dict = dict,
    default_lang = default_lang
  )
}
