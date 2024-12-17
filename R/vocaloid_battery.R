#' Battery
#'
#' @inheritParams last_page_redirect
#'
#' @param languages (character vector) languages for the battery of questionnaires
vocaloid_battery <- function(title = "vocaloidproject",
                             admin_password = "vocaloid",
                             researcher_email = NULL,
                             demo = FALSE,
                             dict = vocaloidproject::vocaloidproject_dict,
                             allow_any_p_id_url = TRUE,
                             force_p_id_from_url = FALSE,
                             languages = c("de_f", "ja"),
                             back_link = NA_character_,
                             gms_subscales = c("Abilities"),
                             logo = NULL) {

  elts <-
    psychTestR::join(
      info_page(
        dict = dict
      ),
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
