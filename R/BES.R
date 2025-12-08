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
    psychTestR::code_block(
      fun = function(state, ...) {
        raw_bes <- as.list(
          psychTestR::get_results(state = state, complete = FALSE)
        )[[label]] %>%
          stringr::str_extract("([0-9]){4}$")
        scoring_map <- vocaloidproject::scoring_maps[["BES"]]
        affective <-
          purrr::map_dbl(
            scoring_map$item[scoring_map$factor == "affective"],
            function(q) {
              x <- raw_bes[[paste0("BES_", q)]] %>% as.numeric()
              eval(
                str2expression(
                  scoring_map$reversed[scoring_map$item == q]
                )
              )
            }
          ) |> sum()
        cognitive <-
          purrr::map_dbl(
            scoring_map$item[scoring_map$factor == "cognitive"],
            function(q) {
              x <- raw_bes[[paste0("BES_", q)]] %>% as.numeric()
              eval(
                str2expression(
                  scoring_map$reversed[scoring_map$item == q]
                )
              )
            }
          ) |> sum()
        psychTestR::save_result(
          place = state,
          label = "affective",
          value = affective
        )
        psychTestR::save_result(
          place = state,
          label = "cognitive",
          value = cognitive
        )
      },
      next_elt = TRUE
    ),
    psychTestR::end_module()
  )
}
