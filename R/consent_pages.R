#' Consent pages
#'
#' @description
#' These functions generate pages for participants to provide informed consent.
#'
#' @details
#' `consent_page()` provides a basic interface for generating consent forms
#' with radio buttons.
#' `consent_page_international1()` uses `consent_page()`, sets the correct
#' defaults for use in [international1()] and adds a redirect to Prolific for
#' participants who do not consent.
#'
#' @param dict internationalization dictionary.
#'
#' @param consent_text_key (scalar character) key in `dict`.
#' Information text for informed consent.
#'
#' @param consent_give_key (scalar character) key in `dict`.
#' Label for the radio button to provide consent.
#'
#' @param consent_no_key (scalar character) key in `dict`.
#' Label for the radio button indicating no consent.
#'
#' @param default_lang Default language for the page.
#'
#' @rdname consent_pages
#'
#' @order 1
#'
#' @export
consent_page <- function(dict = vocaloidproject::vocaloidproject_dict,
                         consent_text_key = "",
                         consent_give_key = "consent_give",
                         consent_no_key = "consent_no",
                         default_lang = "de_f") {

  stopifnot(is.scalar.character(consent_text_key),
            is.scalar.character(consent_give_key),
            is.scalar.character(consent_no_key),
            is.scalar.character(default_lang))

  psychTestR::new_timeline(
    psychTestR::join(
      psychTestR::radiobutton_NAFC_page(
        label = "consent",
        prompt = psychTestR::i18n(consent_text_key),
        labels = c(
          psychTestR::i18n(consent_give_key),
          psychTestR::i18n(consent_no_key)
        ),
        choices = c("gave_consent", "no_consent"),
        trigger_button_text = psychTestR::i18n("CONTINUE"),
        failed_validation_message = psychTestR::i18n("CHOOSE_ANSWER"),
        save_answer = TRUE,
        hide_response_ui = FALSE
      )#,
      # psychTestR::elt_save_results_to_disk(complete = FALSE)
    ),
    dict = dict,
    default_lang = default_lang
  )
}

#' @rdname consent_pages
#'
#' @order 2
#'
#' @inheritParams last_page_redirect
#'
#' @param no_consent_back_link link containing the placeholder `%s` for `p_id`
#' to redirect participants when they do not provide consent.
#'
#' @export
consent_page_international1 <- function(dict = vocaloidproject::vocaloidproject_dict,
                                        no_consent_back_link = "%s",
                                        debug = FALSE) {

  stopifnot(is.scalar.character(no_consent_back_link),
            is.scalar.logical(debug))

  elts <-
    psychTestR::join(
      consent_page(dict = dict,
                   consent_text_key = "consent_text_international1",
                   consent_give_key = "consent_give",
                   consent_no_key = "consent_no"),
      psychTestR::conditional(
        test = function(state, ...) {
          results <-
            psychTestR::get_results(
              state = state,
              complete = FALSE
            )
          # browser()
          consent <-
            as.list(results)$results$consent
          consent == "no_consent"
        },
        logic = last_page_redirect(
          redirect_heading = NULL,
          redirect_paragraph = NULL,
          dict = dict,
          back_link = no_consent_back_link,
          back_link_key = "return_to_prolific",
          debug = debug
        )
      )
    )
  elts
}
