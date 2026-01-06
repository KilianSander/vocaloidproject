#' Experiment 4 (international1)
#'
#' psychTestR battery for Experiment 4 (International Experiment 1) in
#' 3 sessions (see [international1()] for a 2-session variant).
#' It is designed to redirect to SoSci Survey for the stimulus ratings and
#' some questionnaires that look much prettier there.
#'
#' @param technical_error_back_link (character scalar) link for returning to
#' Prolific when URL parameter `uses` or `udes` is missing.
#' It has to contain the placeholder `%s` for `p_id` to redirect
#' participants correctly.
#'
#' @param no_consent_back_link (character scalar) link for redirecting
#' participants who do not give consent.
#'
#' @param follow_up_link (character scalar) link for redirecting participants
#' to the next section of the experiment (on a different platform).
#' It has to contain 3 placeholders, `%s`, for `p_id`, `uses`, and `udes`.
#' The URL parameter for language, `language_url_param`, and its placeholder
#' are appended automatically.
#'
#' @param unmet_requirements_back_link (character scalar) link for redirecting
#' participants when they do not meet study requirements.
#'
#' @param debug (logical scalar) `r lifecycle::badge("experimental")` whether
#' or not to display debug information.
#'
#' @param researcher_email_language_list (named character vector or `NULL`)
#' provide language specific researcher emails as vector elements.
#' Names have to match the language codes.
#' If non-`NULL`, `researcher_email_language_list` takes precedence over
#' `researcher_email`.
#'
#' @param gms_subscales (character vector) passed to the `subscales` argument of
#' [psyquest::GMS()].
#'
#' @inheritParams emotional_baseline_page
#' @inheritParams psyquest::DEG
#' @inheritParams psychTestR::test_options
#' @inheritParams last_page_redirect_session_design
#' @inheritParams session_design_url_welcome_page
#'
#'
experiment4 <- function(title = "",
                        dict = vocaloidproject::vocaloidproject_dict,
                        session_number = 1,
                        debug = FALSE,
                        technical_error_back_link = "",
                        no_consent_back_link = "",
                        unmet_requirements_back_link = "",
                        year_range = c(1955, 2007),
                        gms_subscales = c("General"),
                        follow_up_link = "",
                        admin_password = "vocaloid",
                        researcher_email = "kilian.vogt@hmtm-hannover.de",
                        researcher_email_language_list = NULL,
                        languages = c("de_f", "ja"),
                        logo = NULL,
                        language_url_param = "l",
                        language_url_codes = c("de" = "ger", "de_f" = "ger", "en" = "eng", "ja" = "jpn")) {
  elts <-
    psychTestR::join(
      experiment4_session_start(
        dict = dict,
        session_number = session_number,
        debug = debug,
        technical_error_back_link = technichal_error_back_link,
        no_consent_back_link = no_consent_back_link
      ),
      ## Session 1 ----
      psychTestR::conditional(
        test = function(state, ...) {
          psychTestR::get_global(key = "uses", state = state) == 1
        },
        logic = psychTestR::join(
          international1_session_1(
            dict = dict,
            unmet_requirements_back_link = unmet_requirements_back_link,
            year_range = year_range,
            debug = debug
          )
        )
      ),
      ## Session 2 ----
      psychTestR::conditional(
        test = function(state, ...) {
          psychTestR::get_global(key = "uses", state = state) == 2
        },
        logic = psychTestR::join(
          ASA(
            dict = dict
          ),
          psyquest::GMS(
            dict = dict,
            subscales = gms_subscales
          )
        )
      ),
      ## Session 3 ----
      psychTestR::conditional(
        test = function(state, ...) {
          psychTestR::get_global(key = "uses", state = state) == 3
        },
        logic = psychTestR::join(
          BES(
            dict = dict
          )
        )
      ),
      ## every session ----
      experiment4_every_session(
        dict = dict,
        debug = debug
      ),
      # hack session number
      psychTestR::code_block(
        fun = function(state, ...) {
          uses <- psychTestR::get_global(
            key = "uses",
            state = state
          )
          uses <- paste0(uses, "3")
          psychTestR::set_global(
            key = "uses",
            value = uses,
            state = state
          )
        }
      ),
      ## redirect Session 1 ----
      # goes to picture rating
      psychTestR::conditional(
        test = function(state, ...) {
          psychTestR::get_global(key = "uses", state = state) == 13
        },
        logic = last_page_redirect_session_design(
          redirect_heading = NULL,
          redirect_paragraph = "picinfo",
          dict = dict,
          back_link = follow_up_link,
          language_url_param = language_url_param,
          language_url_codes = language_url_codes,
          back_link_key = "CONTINUE",
          debug = debug
        )
      ),
      ## redirect Session 2 ----
      # goes to example rating
      psychTestR::conditional(
        test = function(state, ...) {
          psychTestR::get_global(key = "uses", state = state) == 23
        },
        logic = last_page_redirect_session_design(
          redirect_heading = NULL,
          redirect_paragraph = "info_example_rating",
          dict = dict,
          back_link = follow_up_link,
          language_url_param = language_url_param,
          language_url_codes = language_url_codes,
          back_link_key = "CONTINUE",
          debug = debug
        )
      ),
      ## redirect Session 3 ----
      # goes to roSAS
      psychTestR::conditional(
        test = function(state, ...) {
          psychTestR::get_global(key = "uses", state = state) == 33
        },
        logic = last_page_redirect_session_design(
          redirect_heading = NULL,
          redirect_paragraph = NULL, # insert dict key !!!
          dict = dict,
          back_link = follow_up_link,
          language_url_param = language_url_param,
          language_url_codes = language_url_codes,
          back_link_key = "CONTINUE",
          debug = debug
        )
      )
    )
  psychTestR::make_test(
    elts = elts,
    opt = psychTestR::test_options(
      title = title,
      admin_password = admin_password,
      researcher_email = researcher_email,
      demo = FALSE,
      languages = languages,
      problems_info = if (is.null(researcher_email_language_list)) {
        problems_info(researcher_email)
      } else {
        problems_info_language_mails(researcher_email_language_list)
      },
      allow_any_p_id_url = TRUE,
      force_p_id_from_url = FALSE,
      logo = logo,
      logo_width = "300px",
      logo_height = "auto"
    )
  )
}

experiment4_session_start <- function(dict = vocaloidproject::vocaloidproject_dict,
                                      session_number = 1,
                                      debug = FALSE,
                                      technical_error_back_link = "",
                                      no_consent_back_link = "") {
  elts <-
    psychTestR::join(
      design_url_welcome_page(
        dict = dict,
        session_number = session_number,
        session_number_max = 3,
        debug = debug
      ),
      psychTestR::conditional(
        test = function(state, ...) {
          any(
            purrr::map_lgl(
              c("uses", "udes"),
              function(x) {
                is.null(psychTestR::get_global(key = x, state = state))
              }
            )
          )
        },
        logic = last_page_redirect(
          redirect_heading = "technical_error",
          back_link_key = "return_to_prolific",
          back_link = technical_error_back_link,
          back_link_with_p_id = FALSE,
          debug = debug
        )
      ),
      # UPDATE consent page !!!
      consent_page_international1(
        dict = dict,
        no_consent_back_link = no_consent_back_link,
        back_link_with_p_id = FALSE,
        debug = debug
      ),
      psychTestR::code_block(
        fun = function(state, ...) {
          psychTestR::save_result(
            place = state,
            label = "udes",
            value = psychTestR::get_global(key = "udes", state = state)
          )
          psychTestR::save_result(
            place = state,
            label = "uses",
            value = session_number
          )
        },
        next_elt = TRUE
      )
    )
  elts
}

experiment4_every_session <- function(dict = vocaloidproject::vocaloidproject_dict,
                                      debug = FALSE) {
  elts <-
    psychTestR::join(
      use_landscape_page(
        img_src = "https://s3.eu-west-1.amazonaws.com/media.dots.org/img/mobile-device-landscape-small.png",
        img_width = 350,
      ),
      emotional_baseline_single_pages(
        dict = dict
      ),
      # loudness HALT, no HP/LS check
      HALT::HALT(
        config = HALT::make_config(
          volume_level = "-20.0 LUFS",
          loop_exclude = 5L,
          channel_check = FALSE,
          screening_parts = FALSE,
          frequency_check = FALSE
        ),
        dict = dict
      ),
      psychTestR::elt_save_results_to_disk(complete = TRUE)
    )
  elts
}
