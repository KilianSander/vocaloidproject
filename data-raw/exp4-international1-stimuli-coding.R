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
condition_key = c(
  # matches condition numbers in `data-raw/createDesign.py` by Elke
  "INST" = 1,
  "FULL_HS_h" = 2,
  "FULL_HS_a" = 3,
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
          "%s%02i.mp3",
          condition, excerpt
        )
      }
    ),
    coded_stimulus_path = file.path(
      stim_dir, coded_stimulus_name
    )
  )
# rename stimuli files
file.rename(
  from = stimuli_international1$original_stimulus_path,
  to = stimuli_international1$coded_stimulus_path
)
# save/export
exp4_international1_stimuli <-
  stimuli_international1 %>%
  dplyr::select(dplyr::contains("name"))
write.csv(
  x = exp4_international1_stimuli,
  file = "data-raw/exp4-international1-stimuli.csv",
  fileEncoding = "UTF-8",
  row.names = FALSE
)
usethis::use_data(
  exp4_international1_stimuli,
  overwrite = TRUE,
  internal = FALSE
)
