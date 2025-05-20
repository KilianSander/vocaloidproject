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
