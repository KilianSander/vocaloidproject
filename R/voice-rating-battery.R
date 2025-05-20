voice_rating_battery <- function(title = "Voiceratingbattery",
                                 admin_password = "vocaloid",
                                 researcher_email = NULL,
                                 demo = FALSE,
                                 dict = vocaloidproject::vocaloidproject_dict,
                                 allow_any_p_id_url = TRUE,
                                 force_p_id_from_url = FALSE,
                                 languages = c("de_f"),
                                 stimulus_url = character(),
                                 logo = NULL,
                                 debug = FALSE) {
  info_p1 <-
    shiny::HTML(
      "Voice rating info"
    )
  info_plast <-
    shiny::HTML(
      "This is the last page."
    )
  elts <-
    psychTestR::join(
      psychTestR::one_button_page(
        body = info_p1,
        button_text = "Weiter"
      ),
      psyquest::DEG(
        subscales = c("Age", "Gender"),
        # show_month = FALSE,
        year_range = c(1925, 2007),
        dict = dict
      ),
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
      psychTestR::elt_save_results_to_disk(complete = TRUE),
      audio_vas_page_battery(
        battery_label = "voice_rating",
        prompt = "Wie klingt diese Stimme?",
        min_label = "künstlich", max_label = "menschlich",
        min_numeric = 1, max_numeric = 100, value = 50.5,
        num_stimuli = 2, stimulus_prefix_pattern = "s%02d",
        base_url = "https://s3.eu-west-1.amazonaws.com/media.gold-msi.org/test_materials/GAR/EMO1",
        randomise_at_runtime = FALSE,
        type = "wav", btn_play_prompt = "Abspielen", button_text = "Weiter",
        step = 1, hide_numeric_values = TRUE, round = TRUE
      ),
      psychTestR::final_page(
        body = info_plast
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
