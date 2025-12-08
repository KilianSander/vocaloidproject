#' Animism Scale for Adults (ASA)
#'
#' This function defines an Animism Scale for Adults (ASA) module for
#' incorporation into a psychTestR timeline.
#' The ASA module uses two of the three original factors of the ASA
#' (池内 \[Ikeuchi\], 2010)
#'
#' @param label (character scalar) label given to the results section.
#'
#' @references
#' 池内裕美. (2010). 成人のアニミズム的思考: 自発的喪失としてのモノ供養の心理.
#' *社会心理学研究*, *25*(3), 167--177.
#' https://doi.org/10.14966/jssp.KJ00006203282
#' \[Ikeuchi, H. (2010). Animistic thinking in adults: The memorial service for
#' dolls as a voluntary loss. *The Japanese Journal of Social Psychology*,
#' *25*(3), 167--177.\]
#'
#' @inheritParams education_page
#'
#' @export
ASA <- function(label = "ASA",
                dict = vocaloidproject::vocaloidproject_dict,
                default_lang = "de_f") {
  stopifnot(is.scalar.character(label))

  psychTestR::join(
    psychTestR::begin_module(label = label),
    psychTestR::new_timeline(
      purrr::map(
        # replace by scoring_maps[["ASA"]]$item_no
        c(1,3,5,2,4,6),
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
