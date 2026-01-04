#' Welcome page with URL parameter retrieval
#'
#' These functions create a welcome page.
#' They check whether the URL parameter `udes` (for design) is present and
#' in the case of `session_design_url_welcome_page()`, whether `uses`
#' (for session) is also present.
#' After completing the page, each URL parameter is set as a global variable
#' (with [psychTestR::set_global()] in the `on_complete` argument of the page).
#'
#' @inheritParams emotional_baseline_page
#'
#' @param debug (logical scalar) whether or not to display the URL parameter
#' `udes` (i.e., the design) on the page.
#'
#' @rdname session_design_url_welcome_page
#' @order 2
#'
#' @export
session_design_url_welcome_page <- function(dict = vocaloidproject::vocaloidproject_dict,
                                            default_lang = "de_f",
                                            debug = FALSE) {
  psychTestR::new_timeline(
    psychTestR::reactive_page(
      function(state, ...) {
        url_params <- psychTestR::get_url_params(state)
        psychTestR::one_button_page(
          if (any(purrr::map_lgl(c("uses", "udes"), function(x) is.null(url_params[[x]])))) {
            shiny::p(psychTestR::i18n("technical_error"))
          } else {
            shiny::div(
              shiny::h3(
                psychTestR::i18n("WELCOME")
              ),
              if (debug) {
                shiny::p(sprintf("Design %s", url_params$udes))
              },
              shiny::p(
                psychTestR::i18n(paste0("SESSION", url_params$uses, "OF2"))
              )
            )
          },
          button_text = psychTestR::i18n("CONTINUE"),
          on_complete = function(state, ...) {
            psychTestR::set_global(key = "uses", value = url_params$uses, state = state)
            psychTestR::set_global(key = "udes", value = url_params$udes, state = state)
            # purrr::walk(c("uses", "udes"), function(x) {psychTestR::set_global(key = x, value = url_params$x, state = state)})
          }
        )
      }
    ),
    dict = dict,
    default_lang = default_lang
  )
}

#' @rdname session_design_url_welcome_page
#' @order 1
#'
#' @param session_number (integer-like scalar) sets the session number.
#' @export
design_url_welcome_page <- function(dict = vocaloidproject::vocaloidproject_dict,
                                    default_lang = "de_f",
                                    session_number = 1,
                                    session_number_max = 2,
                                    debug = FALSE) {
  stopifnot(is.scalar.integerlike(session_number),
            is.scalar.integerlike(session_number_max))
  psychTestR::new_timeline(
    psychTestR::reactive_page(
      function(state, ...) {
        url_params <- psychTestR::get_url_params(state)
        psychTestR::one_button_page(
          if (is.null(url_params[["udes"]])) {
            shiny::p(psychTestR::i18n("technical_error"))
          } else {
            shiny::div(
              shiny::h3(
                psychTestR::i18n("WELCOME")
              ),
              if (debug) {
                shiny::p(sprintf("Design %s", url_params$udes))
              },
              shiny::p(
                psychTestR::i18n(paste0("SESSION", session_number, "OF", session_number_max))
              )
            )
          },
          button_text = psychTestR::i18n("CONTINUE"),
          on_complete = function(state, ...) {
            psychTestR::set_global(key = "uses", value = session_number, state = state)
            psychTestR::set_global(key = "udes", value = url_params$udes, state = state)
          }
        )
      }
    ),
    dict = dict,
    default_lang = default_lang
  )
}
