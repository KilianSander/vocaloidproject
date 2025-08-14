#' Last page of psychTestR questionnaire
#'
#' Generate the last page for a `psychTestR` questionnaire.
#'
#' @param dict internationalization dictionary.
#'
#' @param default_lang (scalar character) default language.
#'
#' @param back_link link containing the placeholder `%s` for `p_id` to redirect
#' participants.
#'
#' @param back_link_key (scalar character) Key in `dict`. Its translation is
#' the text shown for `back_link`.
#'
#' @param redirect_heading (scalar character) Key `dict`. Its translation is
#' shown as the heading of this page.
#'
last_page_redirect <- function(redirect_heading = "thanks",
                               dict = vocaloidproject::vocaloidproject_dict,
                               default_lang = "de_f",
                               back_link,
                               back_link_key = "return_to_prolific") {
  stopifnot(is.scalar.character(back_link_key))

  psychTestR::new_timeline(
    psychTestR::reactive_page(
      fun = function(state, ...) {
        res <-
          psychTestR::get_session_info(
            state,
            complete = TRUE
          )$p_id
        message(sprintf("p_id: %s", res))
        back_link <-
          sprintf(
            back_link,
            res
          )
        psychTestR::final_page(
          body =
            shiny::div(
              shiny::h3(psychTestR::i18n(redirect_heading)),
              shiny::a(
                psychTestR::i18n(back_link_key),
                href = back_link,
                style = "font-size:large;color:red"
              )
            )
        )
      }
    ),
    dict = dict,
    default_lang = default_lang
  )
}
