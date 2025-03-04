#' Set stimuli order
#'
#'
stimuli_order <- function(...) {
  psychTestR::code_block(
    function(state, ...) {
      res <-
        psychTestR::get_results(
          state,
          complete = TRUE,
          add_session_info = TRUE
        ) %>%
        as.list()
      seed <-
        res$sessions$p_id %>%
        digest::sha1() %>%
        charToRaw() %>%
        as.integer() %>%
        sum()
      set.seed(seed)
      stimuli_set <-
        sample(
          letters[1:4],
          size = 1L
        )
      message(stimuli_set)
      psychTestR::save_result(
        label = "stimuli_order",
        value = stimuli_set,
        place = state
      )
    },
    next_elt = TRUE
  )
}
