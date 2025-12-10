#' Basic Empathy Scale (BES)
#'
#' This function defines a Basic Empathy Scale (BES) module for incorporation
#' into a psychTestR timeline.
#' The module uses the BES as validated by Heynen et al. (2016).
#'
#' @references Heynen, E. J. E., Van Der Helm, G. H. P., Stams, G. J. J. M., &
#' Korebrits, A. M. (2016). Measuring empathy in a german youth prison:
#' A validation of the german version of the Basic Empathy Scale (BES)
#' in a sample of incarcerated juvenile offenders.
#' *Journal of Forensic Psychology Practice*, *16*(5), 336--346.
#' <https://doi.org/10.1080/15228932.2016.1219217>
#'
#' @inheritParams ASA
#'
#' @export
BES <- function(label = "BES",
                dict = vocaloidproject::vocaloidproject_dict,
                default_lang = "de_f") {
  stopifnot(is.scalar.character(label))

  psychTestR::join(
    psychTestR::begin_module(label = label),
    psychTestR::new_timeline(
      purrr::map(
        vocaloidproject::scoring_maps$BES$item_no,
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
    psychTestR::code_block(
      fun = function(state, ...) {
        raw_bes <-
          get_scale_results(
            label = label,
            state = state
          )
          # as.list(
          #   psychTestR::get_results(
          #     state = state,
          #     complete = FALSE
          #   )
          # )[[label]] %>%
          # stringr::str_extract("([0-9]){4}$")
        # looking for the error
        scoring_map <- vocaloidproject::scoring_maps[[label]]
        subscale <- "affective"
        raw_res <- raw_bes
        affective <-
        #   get_subscale_score(
        #     raw_bes,
        #     label = "BES",
        #     subscale = "affective"
        #   )
          purrr::map_dbl(
            scoring_map$item[
              stringr::str_detect(scoring_maps[[label]]$factor, subscale)
            ],
            function(q) {
              x <- raw_res[[paste0(label, "_", q)]] %>% as.numeric()
              eval(
                str2expression(
                  scoring_map$reversed[scoring_map$item == q]
                )
              )
            }
          ) %>% sum()
        # cognitive <-
        #   get_subscale_score(
        #     raw_bes,
        #     label = "BES",
        #     subscale = "cognitive"
        #   )
        psychTestR::save_result(
          place = state,
          label = "affective",
          value = affective
        )
        # psychTestR::save_result(
        #   place = state,
        #   label = "cognitive",
        #   value = cognitive
        # )
      },
      next_elt = TRUE
    ),
    psychTestR::end_module()
  )
}
