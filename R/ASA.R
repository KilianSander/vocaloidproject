#' ASA questionnaire
#'
#' @param label (character scalar) label given to the results section.
#'
#' @inheritParams education_page
ASA <- function(label = "ASA",
                dict = vocaloidproject::vocaloidproject_dict,
                default_lang = "de_f") {
  stopifnot(is.scalar.character(label))

  psychTestR::join(
    psychTestR::begin_module(label = label),
    psychTestR::new_timeline(
      purrr::map(
        1:6,
        function(x) {
          psychTestR::NAFC_page(
            label = paste0(label, "_q", x),
            prompt = psychTestR::i18n(sprintf("ASA_%04i_PROMPT", x)),
            choices = sprintf("ASA_CHOICE_%04i", 1:4),
            labels = sapply(
              sprintf("ASA_CHOICE_%04i", 1:4), psychTestR::i18n,
              simplify = TRUE, USE.NAMES = FALSE
            ),
            save_answer = TRUE
          )
        }
      ),
      dict = dict,
      default_lang = default_lang
    ),
    # scoring?
    psychTestR::end_module()
  )
}
