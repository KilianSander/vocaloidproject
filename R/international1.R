#' Battery for International Experiment 1
#'
#'
#' @param gms_subscales (character vector) passed to the `subscales` argument of
#' [psyquest::GMS()].
#'
#' @param technical_error_back_link (character scalar) link for returning to
#' Prolific when URL parameter `uses` or `udes` is missing.
#' It has to contain the placeholder `%s` for `p_id` to redirect
#' participants correctly.
#'
#' @param follow_up_link (character scalar) link for redirecting participants
#' to the next section of the experiment (on a different platform).
#' It has to contain 3 placeholders, `%s`, for `p_id`, `uses`, and `udes`.
#' The URL parameter for language, `language_url_param`, and its placeholder
#' are appended automatically.
#'
#' @param debug (logical scalar) `r lifecycle::badge("experimental")` whether
#' or not to display debug information.
#'
#' @inheritParams emotional_baseline_page
#' @inheritParams psyquest::DEG
#' @inheritParams psychTestR::test_options
#' @inheritParams last_page_redirect_session_design
#' @inheritParams session_design_url_welcome_page
#'
#' @export
international1 <- function(title = "",
                           admin_password = "vocaloid",
                           researcher_email = "kilian.vogt@hmtm-hannover.de",
                           dict = vocaloidproject::vocaloidproject_dict,
                           session_number = 1,
                           allow_any_p_id_url = TRUE,
                           force_p_id_from_url = FALSE,
                           languages = c("de_f", "ja"),
                           year_range = c(1955, 2007),
                           gms_subscales = c("General"),
                           logo = NULL,
                           technical_error_back_link = "",
                           follow_up_link = "",
                           language_url_param = "l",
                           language_url_codes = c("de" = "ger", "de_f" = "ger", "en" = "eng", "ja" = "jpn"),
                           debug = FALSE){
  stopifnot(is.character(languages),
            is.scalar.character(technical_error_back_link),
            is.scalar.character(follow_up_link),
            is.scalar.logical(debug))
  #
  elts <-
    psychTestR::join(
      design_url_welcome_page(
        dict = dict,
        session_number = session_number,
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
          debug = debug
        )
      ),
      # consent page
      consent_page_international1(
        dict = dict,
        no_consent_back_link = technical_error_back_link,
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
      ),
      psychTestR::new_timeline(
        psychTestR::one_button_page(
          body = psychTestR::i18n("info_text"),
          button_text = psychTestR::i18n("CONTINUE")
        ),
        dict = dict,
        default_lang = "de_f"
      ),
      psychTestR::conditional(
        test = function(state, ...) {
          psychTestR::get_global(key = "uses", state = state) == 1
        },
        logic = international1_session_1(
          dict = dict,
          year_range = year_range
        )
      ),
      psychTestR::conditional(
        test = function(state, ...) {
          psychTestR::get_global(key = "uses", state = state) == 2
        },
        logic = psyquest::GMS(
          dict = dict,
          subscales = gms_subscales
        )
      ),
      # emotional_baseline_page(
      #   dict = dict,
      #   vertical = FALSE # DOES NOT LOOK PRETTY ON MOBILE DEVICES!!
      # ),
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
      psychTestR::elt_save_results_to_disk(complete = TRUE),
      # preliminary
      psychTestR::conditional(
        test = function(state, ...) {
          psychTestR::get_global(key = "uses", state = state) == 1
        },
        logic = last_page_redirect_session_design(
          redirect_heading = NULL,
          redirect_paragraph = "picinfo",
          dict = dict,
          back_link = follow_up_link,
          back_link_key = "CONTINUE",
          debug = TRUE
        )
      ),
      # session 2
      international1_session2_handshake_page(
        dict = dict,
        follow_up_link = follow_up_link,
        back_link_key = "CONTINUE",
        debug = TRUE
      )
    )
  #
  psychTestR::make_test(
    elts = elts,
    opt = psychTestR::test_options(
      title = title,
      admin_password = admin_password,
      researcher_email = researcher_email,
      demo = FALSE,
      languages = languages,
      problems_info = problems_info(researcher_email),
      allow_any_p_id_url = allow_any_p_id_url,
      force_p_id_from_url = force_p_id_from_url,
      logo = logo,
      logo_width = "300px",
      logo_height = "auto"
    )
  )
}

international1_session_1 <- function(dict = vocaloidproject::vocaloidproject_dict,
                                     year_range = c(1925, 2007)) {
  psychTestR::join(
    psyquest::DEG(
      subscales = c("Age", "Gender"),
      # show_month = FALSE,
      year_range = year_range,
      dict = dict
    ),
    psychTestR::begin_module(label = "demographics"),
    first_language_page(
      dict = dict
    ),
    education_page(
      dict = dict
    ),
    student_page(
      dict = dict
    ),
    field_page(
      dict = dict
    ),
    psychTestR::conditional(
      test = function(state, ...) {
        psychTestR::get_session_info(state, complete = FALSE)$language != "ja"
      },
      logic = japanese_skills_page(
        dict = dict
      )
    ),
    psychTestR::end_module()
  )
}

international1_session2_handshake_page <- function(dict = vocaloidproject::vocaloidproject_dict,
                                                   follow_up_link = "",
                                                   back_link_key = "CONTINUE",
                                                   debug = FALSE) {
  last_page_redirect_session_design(
    redirect_heading = NULL,
    redirect_paragraph = "info_example_rating",
    dict = dict,
    back_link = follow_up_link,
    back_link_key = back_link_key,
    debug = debug
  )
}
