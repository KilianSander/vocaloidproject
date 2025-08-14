#' Last page of psychTestR questionnaire
#'
#' Generate the last page for a `psychTestR` questionnaire including a redirect
#' link.
#'
#' @details
#' `p_id` is passed on as a URL parameter.
#' `last_page_redirect_session_design()` also passes on `uses` (session)
#' and `udes` (design).
#'
#' @param dict internationalization dictionary.
#'
#' @param default_lang (scalar character) default language.
#'
#' @param back_link link containing the placeholder `%s` for `p_id` to redirect
#' participants.
#' In the case of `last_page_redirect_session_design` two additional
#' placeholders for session and design have to be included.
#'
#' @param back_link_key (scalar character) Key in `dict`. Its translation is
#' the text shown for `back_link`.
#'
#' @param redirect_heading (scalar character or `NULL`) Key in `dict`.
#' Its translation is shown as the heading of this page.
#' `NULL` suppresses the heading.
#'
#' @param debug (logical scalar) `r lifecycle::badge("experimental")` whether or
#' not to `message()` the URL parameter(s).
#'
last_page_redirect <- function(redirect_heading = "thanks",
                               dict = vocaloidproject::vocaloidproject_dict,
                               default_lang = "de_f",
                               back_link,
                               back_link_key = "return_to_prolific",
                               debug = FALSE) {
  stopifnot(is.scalar.character(redirect_heading) | is.null(redirect_heading),
            is.scalar.character(back_link_key),
            stringr::str_count(back_link, pattern = "%s") == 1,
            is.scalar.logical(debug))

  psychTestR::new_timeline(
    psychTestR::reactive_page(
      fun = function(state, ...) {
        res <-
          psychTestR::get_session_info(
            state,
            complete = TRUE
          )$p_id
        if (debug) message(sprintf("p_id: %s", res))
        back_link <-
          sprintf(
            back_link,
            res
          )
        psychTestR::final_page(
          body =
            shiny::div(
              if (!is.null(redirect_heading)) {
                shiny::h3(psychTestR::i18n(redirect_heading))
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
last_page_redirect_session_design <- function(redirect_heading = "thanks",
                                              dict = vocaloidproject::vocaloidproject_dict,
                                              default_lang = "de_f",
                                              back_link,
                                              back_link_key = "CONTINUE",
                                              debug = FALSE) {
  stopifnot(is.scalar.character(redirect_heading) | is.null(redirect_heading),
            is.scalar.character(back_link_key),
            stringr::str_count(back_link, pattern = "%s") == 3,
            is.scalar.logical(debug))

  psychTestR::new_timeline(
    psychTestR::reactive_page(
      fun = function(state, ...) {
        pid <-
          psychTestR::get_session_info(
            state,
            complete = TRUE
          )$p_id
        uses <- psychTestR::get_global(key = "uses", state = state)
        udes <- psychTestR::get_global(key = "udes", state = state)
        if (debug) message(sprintf("p_id: %s\nSession: %s\nDesign: %s", pid, uses, udes))
        back_link <-
          sprintf(
            back_link,
            pid, uses, udes
          )
        psychTestR::final_page(
          body =
            shiny::div(
              if (!is.null(redirect_heading)) {
                shiny::h3(psychTestR::i18n(redirect_heading))
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
