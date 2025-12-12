get_scale_results <- function(label, state) {
  res <-
    as.list(
      psychTestR::get_results(
        state = state,
        complete = FALSE
      )
    )[[label]] %>%
    stringr::str_extract("([0-9]){4}$")
  res
}

get_subscale_score <- function(raw_res, label, subscale) {
  scoring_map <- vocaloidproject::scoring_maps[[label]]
  ret <-
    purrr::map_dbl(
      scoring_map$item[
        stringr::str_detect(scoring_map$factor, subscale)
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
  ret
}
