#' Last page of psychTestR questionnaire
#'
#' Generate the last page for a `psychTestR` questionnaire including a redirect
#' link.
#'
#' @details
#' `p_id` is passed on as a URL parameter.
#' `last_page_redirect_session_design()` also passes on `uses` (session)
#' and `udes` (design).
#' Optionally, you can specify a language URL parameter to pass on
#' the participant's test language.
#'
#' @param dict internationalization dictionary.
#'
#' @param default_lang (scalar character) default language.
#'
#' @param back_link link to redirect participants that depending on
#' `back_link_with_p_id` contains the placeholder `%s` for `p_id`.
#' In the case of `last_page_redirect_session_design` two additional
#' placeholders for session and design have to be included.
#'
#' @param back_link_with_p_id (logical scalar) whether or not `back_link`
#' contains a placeholder for the participant ID.
#'
#' @param back_link_key (scalar character) Key in `dict`. Its translation is
#' the text shown for `back_link`.
#'
#' @param redirect_heading (scalar character or `NULL`) Key in `dict`.
#' Its translation is displayed as the heading of this page.
#' `NULL` suppresses the heading.
#'
#' @param redirect_paragraph (scalar character or `NULL`) Key in `dict`.
#' Its translation is displayed as a paragraph of text on the page.
#' `NULL` suppresses a paragraph.
#'
#' @param debug (logical scalar) `r lifecycle::badge("experimental")` whether or
#' not to `message()` the URL parameter(s).
#'
last_page_redirect <- function(redirect_heading = "thanks",
                               redirect_paragraph = NULL,
                               dict = vocaloidproject::vocaloidproject_dict,
                               default_lang = "de_f",
                               back_link,
                               back_link_key = "return_to_prolific",
                               debug = FALSE,
                               back_link_with_p_id = TRUE) {
  stopifnot(is.scalar.character(redirect_heading) | is.null(redirect_heading),
            is.scalar.character(redirect_paragraph) | is.null(redirect_paragraph),
            is.scalar.character(back_link_key),
            is.scalar.logical(back_link_with_p_id),
            is.scalar.logical(debug))

  if (back_link_with_p_id) {
    if (stringr::str_count(back_link, pattern = "%s") != 1) {
      stop(
        paste0(
          "If `back_link_with_p_id` is set to `TRUE`, ",
          "`back_link` must contain `%s` exactly once."
        )
      )
    }
  }

  psychTestR::new_timeline(
    psychTestR::reactive_page(
      fun = function(state, ...) {
        res <-
          psychTestR::get_session_info(
            state,
            complete = TRUE
          )$p_id
        if (debug) message(sprintf("p_id: %s", res))
        if (back_link_with_p_id) {
          back_link <-
            sprintf(
              back_link,
              res
            )
        }
        psychTestR::final_page(
          body =
            shiny::div(
              if (!is.null(redirect_heading)) {
                shiny::h3(psychTestR::i18n(redirect_heading))
              },
              if (!is.null(redirect_paragraph)) {
                shiny::p(psychTestR::i18n(redirect_paragraph))
              },
              shiny::a(
                psychTestR::i18n(back_link_key),
                href = back_link,
                class = "btn btn-default"
              )
            )
        )
      }
    ),
    dict = dict,
    default_lang = default_lang
  )
}

#' @rdname last_page_redirect
#'
#' @param language_url_param (character scalar or `NULL`) URL parameter to pass
#' on the test language of a participant.
#' If non-`NULL`, the URL parameter will be appended to the `back_link` and
#' the language will be read from the participant's session info.
#'
#' @param language_url_codes (named character vector or `NULL`)
#' A named character vector will be used when `language_url_param` is
#' non-`NULL` to translate the psychTestR language codes
#' (in most cases two lower-case letters according to ISO 639-2 conventions)
#' to the codes for the redirect link
#' (e.g., SoSci Survey uses three letter codes like `ger` for German, `eng` for
#' English, or `jpn` for Japanese).
#' If `NULL`, the character scalar retrieved by
#' [psychTestR::get_session_info()]`$language` is used.
#'
#' @export
last_page_redirect_session_design <- function(redirect_heading = "thanks",
                                              redirect_paragraph = NULL,
                                              dict = vocaloidproject::vocaloidproject_dict,
                                              default_lang = "de_f",
                                              back_link,
                                              back_link_key = "CONTINUE",
                                              language_url_param = "l",
                                              language_url_codes = c("de" = "ger", "de_f" = "ger", "en" = "eng", "ja" = "jpn"),
                                              debug = FALSE) {
  stopifnot(
    is.scalar.character(redirect_heading) | is.null(redirect_heading),
    is.scalar.character(redirect_paragraph) | is.null(redirect_paragraph),
    is.scalar.character(back_link_key),
    stringr::str_count(back_link, pattern = "%s") == 3,
    is.scalar.character(language_url_param) | is.null(language_url_param),
    is.character(language_url_codes) | is.null(language_url_codes),
    is.scalar.logical(debug)
  )

  psychTestR::new_timeline(
    psychTestR::reactive_page(
      fun = function(state, ...) {
        sessinfo <-
          psychTestR::get_session_info(
            state,
            complete = TRUE
          )
        pid <- sessinfo$p_id
        lang <- sessinfo$language
        uses <- psychTestR::get_global(key = "uses", state = state)
        udes <- psychTestR::get_global(key = "udes", state = state)

        if (debug) message(sprintf("p_id: %s\nSession: %s\nDesign: %s", pid, uses, udes))
        back_link <-
          sprintf(
            back_link,
            pid, uses, udes
          )
        if (!is.null(language_url_param)) {
          if (!is.null(language_url_codes)) {
            lang <- language_url_codes[lang]
          }
          back_link <-
            paste0(
              back_link, "&", language_url_param, "=", lang
            )
        }
        psychTestR::final_page(
          body =
            shiny::div(
              if (!is.null(redirect_heading)) {
                shiny::h3(psychTestR::i18n(redirect_heading))
              },
              if (!is.null(redirect_paragraph)) {
                shiny::p(psychTestR::i18n(redirect_paragraph))
              },
              shiny::a(
                psychTestR::i18n(back_link_key),
                href = back_link,
                class = "btn btn-default"
              )
            )
        )
      }
    ),
    dict = dict,
    default_lang = default_lang
  )
}
