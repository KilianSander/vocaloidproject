#' Educational degree
#'
#' This function creates a page to ask for the highest educational degree.
#' The choices are oriented towards the ISCED categories.
#'
#' @inheritParams first_language_page
#'
#' @export
education_page <- function(label = "educational_degree",
                           dict = vocaloidproject::vocaloidproject_dict,
                           default_lang = "de_f") {
  stopifnot(is.scalar.character(label),
            is.character(default_lang))
  psychTestR::new_timeline(
    psychTestR::NAFC_page(
      label = label,
      prompt = psychTestR::i18n("EDU_PROMPT"),
      choices = as.character(1:6),
      labels = purrr::map(
        paste0("EDU_CHOICE", 1:6),
        psychTestR::i18n
      ),
      button_style = "width:400px",
      save_answer = TRUE
    ),
    dict = dict,
    default_lang = default_lang
  )
}

#' University student page
#'
#' This function creates a page to ask participants whether they are currently
#' studying at a university.
#'
#' @inheritParams first_language_page
#'
#' @export
student_page <- function(label = "student",
                         dict = vocaloidproject::vocaloidproject_dict,
                         default_lang = "de_f") {
  stopifnot(is.scalar.character(label),
            is.character(default_lang))
  psychTestR::new_timeline(
    psychTestR::NAFC_page(
      label = label,
      prompt = psychTestR::i18n("STUDENT_PROMPT"),
      choices = c("student", "non-student"),
      labels = purrr::map(
        paste0("STUDENT_CHOICE", 1:2),
        psychTestR::i18n
      ),
      save_answer = TRUE
    ),
    dict = dict,
    default_lang = default_lang
  )
}

field_page <- function(label = "professional_field",
                       dict = vocaloidproject::vocaloidproject_dict,
                       default_lang = "de_f") {
  stopifnot(is.scalar.character(label),
            is.character(default_lang))
  psychTestR::new_timeline(
    psychTestR::NAFC_page(
      label = label,
      prompt = psychTestR::i18n("PROFESSIONALFIELDSTUDY_PROMPT"),
      choices = c("psychology", "music/musician", "musicology",
                  "sound engineering", "other music-related subjects", "other"),
      labels = purrr::map(
        paste0("PROFESSIONALFIELDSTUDY_CHOICE", 1:6),
        psychTestR::i18n
      ),
      save_answer = TRUE,
      button_style = 'width:400px'
    ),
    dict = dict,
    default_lang = default_lang
  )
}
