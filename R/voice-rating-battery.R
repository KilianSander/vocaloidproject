#' psychTestR Battery for Voice Rating Study
#'
#' @inheritParams psychTestR::test_options
#'
#' @param randomise_stimuli_at_runtime (logical scalar) Whether or not to
#' randomise the order of the stimuli (see [audio_vas_page_battery()]).
#'
#' @inheritParams vocaloid_battery
#'
#' @inheritParams audio_vas_page_battery
#'
#' @export
voice_rating_battery <- function(title = "Voiceratingbattery",
                                 admin_password = "vocaloid",
                                 randomise_stimuli_at_runtime = TRUE,
                                 researcher_email = NULL,
                                 demo = FALSE,
                                 dict = vocaloidproject::vocaloidproject_dict,
                                 allow_any_p_id_url = TRUE,
                                 force_p_id_from_url = FALSE,
                                 languages = c("de_f"),
                                 num_stimuli = 2,
                                 stimulus_prefix_pattern = character(),
                                 base_url = character(),
                                 logo = NULL,
                                 debug = FALSE) {
  info_p1 <-
    shiny::HTML(
      "Voice rating info"
    )
  # info_plast <-
  #   shiny::HTML(
  #     "This is the last page."
  #   )
  elts <-
    psychTestR::join(
      psychTestR::new_timeline(
        psychTestR::one_button_page(
          body = info_p1,
          button_text = psychTestR::i18n("CONTINUE")
        ),
        dict = dict
      ),
      psyquest::DEG(
        subscales = c("Age", "Gender"),
        # show_month = FALSE,
        year_range = c(1925, 2007),
        dict = dict
      ),
      psychTestR::begin_module(label = "EDU"),
      student_page(
        dict = dict
      ),
      field_page(
        dict = dict
      ),
      first_language_page(
        dict = dict
      ),
      education_page(
        dict = dict
      ),
      psychTestR::elt_save_results_to_disk(complete = FALSE),
      psychTestR::end_module(),
      psychTestR::new_timeline(
      audio_vas_page_battery(
        battery_label = "voice_rating",
        prompt = psychTestR::i18n("VOICERATING_PROMPT"),
        min_label = psychTestR::i18n("VOICERATING_MIN"),
        max_label = psychTestR::i18n("VOICERATING_MAX"),
        min_numeric = 1, max_numeric = 100, value = 50.5,
        num_stimuli = num_stimuli, stimulus_prefix_pattern = "s%02d",
        base_url = "https://s3.eu-west-1.amazonaws.com/media.gold-msi.org/test_materials/GAR/EMO1",
        randomise_at_runtime = randomise_stimuli_at_runtime,
        type = "wav", btn_play_prompt = "Abspielen", button_text = "Weiter",
        step = 1, hide_numeric_values = TRUE, fill_bar = FALSE, round = TRUE
      ),
      dict = dict
      ),
      psychTestR::elt_save_results_to_disk(complete = TRUE),
      psychTestR::new_timeline(
        psychTestR::final_page(
          body = shiny::div(
            shiny::p(psychTestR::i18n("thanks")),
            shiny::p(
              psychTestR::i18n("RESULTS_SAVED"),
              psychTestR::i18n("CLOSE_BROWSER")
            )
          )
        ),
        dict = dict
      )
    )

  psychTestR::make_test(
    elts = elts,
    opt = psychTestR::test_options(
      title = title,
      admin_password = admin_password,
      researcher_email = researcher_email,
      demo = demo,
      languages = languages,
      allow_any_p_id_url = allow_any_p_id_url,
      force_p_id_from_url = force_p_id_from_url,
      logo = logo,
      logo_width = "300px",
      logo_height = "auto"
    )
  )
}
