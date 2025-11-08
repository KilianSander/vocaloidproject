# Coding for Stimuli in Experiment 4 (International 1) -------------------------
#
# Original file names EXCERPT_CONDITION.mp3
# EXCERPT is an integer from 1 to 30
# CONDITION is a string
#   INST = instrumental only (no main melody)
#   FULL_HS_a = full arrangement with human singer in the artificial version
#   FULL_HS_h = full arrangement with human singer in the human version
#   FULL_VC = full arrangement with vocaloid
#
require(magrittr)
condition_key = c( # CHECK CONDITION NUMBERS !!!
  "INST" = 1,
  "FULL_HS_a" = 2,
  "FULL_HS_h" = 3,
  "FULL_VC" = 4
)
#
# set `stim_dir` to the directory containing the stimuli with their original name
stim_dir <- ""
#
stimuli_international1 <-
  tibble::tibble(
    original_stimulus_path = list.files(
      path = stim_dir,
      pattern = "mp3$",
      full.names = TRUE
    ),
    original_stimulus_filename = basename(original_stimulus_path),
    coded_stimulus_name = purrr::map_chr(
      original_stimulus_filename,
      function(x) {
        excerpt <-
          stringr::str_remove(
            string = x,
            pattern = "_(.*)"
          ) |>
          as.integer()
        condition <-
          stringr::str_replace_all(
            string = x,
            pattern = c(
              "^\\d{1,2}_" = "",
              ".mp3$" = ""
            )
          ) %>%
          condition_key[[.]]
        sprintf(
          "%s%02i",
          condition, excerpt
        )
      }
    )
  )
#
file.rename()
