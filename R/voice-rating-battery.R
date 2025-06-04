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
voice_rating_battery <- function(title = "Wie klingen die Stimmen von Sängern?",
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
    # shiny::HTML(
    #   "Voice rating info"
    # )
    shiny::div(
      shiny::p(
        "Vielen Dank, dass Sie an unserer Studie teilnehmen wollen!",
        "Wir interessieren uns dafür, wie für Sie die Stimmen verschiedener Sänger klingen: künstlich oder menschlich?",
        "Dabei gibt es kein richtig oder falsch.",
        "Sie werden 60 kurze Ausschnitte hören bewerten.",
        "Der Fragebogen startet mit Fragen zu Ihrer Person.",
        "Er dauert insgesamt ca. 45 min [check!!!]."
      ),
      shiny::p(
        "Ihre Teilnahme ist freiwillig.",
        shiny::tags$b("Voraussetzung zur Teilnahme ist, dass Sie mindestens 18 Jahre alt sind, Sie keine fundierten Japanischen Sprachkenntnisse haben, und Deutsch Ihre Muttersprache ist."),
        "Sie können Ihre Teilnahme jederzeit ohne Angabe von Gründen beenden, aber es würde uns helfen, wenn Sie bis zum Ende alle Songs bewerten könnten.",
        "Bei vorzeitigem Abbruch werden Ihre Daten gelöscht."
      ),
      shiny::p(
        "Alle Informationen, die wir durch die Befragung von Ihnen erhalten, werden vertraulich behandelt und sind anonymisiert, d.h. können in keiner Weise zu Ihrer Person zurückverfolgt werden.",
        "Die erhobenen, anonymisierten Daten dienen der Grundlagenforschung und werden sowohl in Fachzeitschriften oder Konferenzpräsentationen veröffentlich, als auch in öffentlich zugänglichen Datenbanken,",
        shiny::HTML("um <q>open science</q>, d.h. die Nachvollziehbarkeit wissenschaftlicher Forschung, zu unterstützen.")
      ),
      shiny::p(
        "Bei Fragen oder Anregungen kontaktieren Sie bitte die Leiterin der Studie Dr. Elke Lange, ",
        shiny::HTML("<a href=elke.lange@ae.mpg.de>elke.lange@ae.mpg.de</a>,"),
        "Senior Researcher, Max-Planck-Institut für Empirische Ästhetik, Frankfurt am Main."
      ),
      shiny::p(
        "Durch das Klicken auf die Schaltfläche",
        shiny::HTML("<q>Ich stimme den Teilnahmebedingungen zu und möchte an der Studie teilnehmen.</q>"),
        "geben Sie Ihr Einverständnis zu den oben genannten Bedingungen."
      )
    )
  info_pre_stimuli <-
    shiny::div(
      shiny::p(
        "Es folgen nun 60 kurze Ausschnitte mit Pop-songs.",
        "Manche der Stimmen könnten künstlicher und andere menschlicher klingen.",
        "Es gibt keine richtigen oder falschen Antworten. Uns interessiert Ihre Meinung.",
        "Bitte überlegen Sie, ob die jeweilige Gesangsstimme für Sie eher künstlich oder eher menschlich klingt.",
        "Sie können Ihr Urteil über die Skala eingeben und dazu den runden Knopf nach links oder rechts verschieben,",
        shiny::HTML("z.&nbsp;B. in Richtung <q>künstlich</q> oder in Richtung <q>menschlich</q>.")
      ),
      shiny::p(
        "Damit es ein wenig abwechslungsreicher ist, haben wir 30 songs gewählt,",
        "bei denen die Sänger auf Japanisch singen und 30,",
        shiny::HTML("bei denen die Sänger Silben wie <q>lalala</q> verwenden.")
      ),
      shiny::p("Nun geht es los!")
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
          button_text = psychTestR::i18n("CONSENT_BUTTON"),
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
      japanese_skills_page(
        dict = dict
      ),
      education_page(
        dict = dict
      ),
      psychTestR::elt_save_results_to_disk(complete = FALSE),
      psychTestR::end_module(),
      psychTestR::new_timeline(
        psychTestR::one_button_page(
          body = shiny::div(
            info_pre_stimuli,
            shinyWidgets::chooseSliderSkin("Square")
          ),
          button_text = psychTestR::i18n("CONTINUE")
        ),
        dict = dict
      ),
      purrr::map(
        c(FALSE, TRUE) -> pick_from,
        function(boolean) {
          psychTestR::new_timeline(
            psychTestR::conditional(
              test = pick_random_on_p_id(
                pick_check = boolean,
                pick_from = pick_from,
                label = "voice_rating_reversed"
              ),
              logic = audio_vas_page_battery(
                battery_label = "voice_rating",
                prompt = psychTestR::i18n("VOICERATING_PROMPT"),
                min_label = psychTestR::i18n("VOICERATING_MIN"),
                max_label = psychTestR::i18n("VOICERATING_MAX"),
                min_numeric = 1, max_numeric = 100, value = 50.5,
                num_stimuli = num_stimuli, stimulus_prefix_pattern = "s%02d",
                base_url = "https://s3.eu-west-1.amazonaws.com/media.gold-msi.org/test_materials/GAR/EMO1",
                randomise_at_runtime = randomise_stimuli_at_runtime,
                type = "wav", btn_play_prompt = "Abspielen", button_text = "Weiter",
                step = 1, hide_numeric_values = TRUE, fill_bar = FALSE, round = TRUE,
                reverse = boolean
              )
            ),
            dict = dict
          )
        }
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
