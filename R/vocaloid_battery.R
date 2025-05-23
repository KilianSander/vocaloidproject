#' Battery
#'
#' @inheritParams last_page_redirect
#'
#' @inheritParams psychTestR::test_options
#'
#' @param gms_subscales (character vector) passed to the `subscales` argument of
#' [psyquest::GMS()].
#'
#' @param debug (logical scalar) `r lifecycle::badge("experimental")`
#'
#' @export
vocaloid_battery <- function(title = "vocaloidproject",
                             admin_password = "vocaloid",
                             researcher_email = NULL,
                             demo = FALSE,
                             dict = vocaloidproject::vocaloidproject_dict,
                             allow_any_p_id_url = TRUE,
                             force_p_id_from_url = FALSE,
                             languages = c("de_f", "ja"),
                             back_link = NA_character_,
                             gms_subscales = c("General"),
                             logo = NULL,
                             debug = FALSE) {

  elts <-
    psychTestR::join(
      info_page(
        dict = dict
      ),
      emotional_baseline_page(dict = dict),
      psyquest::DEG(
        subscales = c("Age", "Gender"),
        # show_month = FALSE,
        year_range = c(1925, 2007),
        dict = dict
      ),
      first_language_page(
        dict = dict
      ),
      education_page(
        dict = dict
      ),
      stimuli_order(),
      psyquest::GMS(
        dict = dict,
        subscales = gms_subscales
      ),
      psychTestR::elt_save_results_to_disk(
        complete = TRUE
      ),
      last_page_redirect(
        dict = dict,
        back_link = back_link
      )
    )
  psychTestR::make_test(
    elts = elts,
    opt = psychTestR::test_options(
      title = "",
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
