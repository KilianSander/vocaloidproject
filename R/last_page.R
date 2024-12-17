#' Last page of psychTestR questionnaire
#'
#' Generate the last page for a `psychTestR` questionnaire.
#'
#' @param dict internationalization dictionary.
#'
#' @param default_lang (scalar character) default language.
#'
#' @param back_link link to redirect participants.
#'
last_page_redirect <- function(dict = vocaloidproject::vocaloidproject_dict,
                               default_lang = "de_f",
                               back_link) {
  psychTestR::new_timeline(
    psychTestR::final_page(
      body =
        shiny::div(
          shiny::h3(psychTestR::i18n("thanks")),
          shiny::a(
            psychTestR::i18n("return_to_prolific"),
            href = back_link,
            style = "font-size:large;color:red"
          )
        )
    ),
    dict = dict,
    default_lang = default_lang
  )
}
