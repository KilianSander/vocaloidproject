#' Basic Empathy Scale (BES)
#'
#' @inheritParams ASA
#'
#'
BES <- function(label = "BES",
                dict = vocaloidproject::vocaloidproject_dict,
                default_lang = "de_f") {
  stopifnot(is.scalar.character(label))

  psychTestR::join(
    psychTestR::begin_module(label = label),
    psychTestR::new_timeline(
      purrr::map(
        c(2, 3, 5, 8:12, 14, 16:18),
        function(x) {
          psychTestR::NAFC_page(
            label = paste0(label, "_q", x),
            prompt = psychTestR::i18n(sprintf("BES_%04i_PROMPT", x)),
            choices = sprintf("BES_CHOICE_%04i", 1:5),
            labels = sapply(
              sprintf("BES_CHOICE_%04i", 1:5), psychTestR::i18n,
              simplify = TRUE, USE.NAMES = FALSE
            ),
            save_answer = TRUE
          )
        }
      ),
      dict = dict,
      default_lang = default_lang
    ),
    # scoring
    psychTestR::end_module()
  )
}
