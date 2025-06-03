# Set stimuli order
#
#
# stimuli_order <- function(...) {
#   psychTestR::code_block(
#     function(state, ...) {
#       res <-
#         psychTestR::get_results(
#           state,
#           complete = FALSE,
#           add_session_info = TRUE
#         ) %>%
#         as.list()
#       seed <-
#         res$sessions$p_id %>%
#         digest::sha1() %>%
#         charToRaw() %>%
#         as.integer() %>%
#         sum()
#       set.seed(seed)
#       stimuli_set <-
#         sample(
#           letters[1:4],
#           size = 1L
#         )
#       message(stimuli_set)
#       psychTestR::save_result(
#         label = "stimuli_order",
#         value = stimuli_set,
#         place = state
#       )
#     },
#     next_elt = TRUE
#   )
# }

stimuli_order_for_participant <- function(stimuli_set) {
  function(state, ...) {
    p_id <-
      psychTestR::get_session_info(state, complete = FALSE)$p_id
    if (is.null(p_id)) {
      message("p_id NULL")
      p_id <- -1
    }
    seed <- p_id_to_seed()
    set.seed(seed)
    random_stimuli_set <-
      sample(
        letters[1:4],
        size = 1
      )
    if (stimuli_set == random_stimuli_set) {
      message(
        sprintf(
          "\np_id: %s\nseed: %d\nstimuli_set: %s\nrandom_stimuli_set: %s\n",
          p_id, seed, stimuli_set, random_stimuli_set
        )
      )
      psychTestR::set_global(
        key = "stimuli_set",
        value = stimuli_set,
        state = state
      )
      psychTestR::save_result(
        place = state,
        label = "stimuli",
        value = stimuli_set
      )
    }
    return(stimuli_set == random_stimuli_set)
  }
}

stimuli_order <- function() {
  psychTestR::join(
    purrr::map(
      letters[1:4],
      function(x) {
        psychTestR::conditional(
          stimuli_order_for_participant(x),
          psychTestR::one_button_page(
            body = paste0("Stimuli set ", x)
          )
        )
      }
    )
  )
}
