## Read design files -----------------------------------------------------------
# require(dplyr)
read_design <- function(file) {
  read.table(
    file = file,
    header = FALSE,
    sep = " ",
    colClasses = c(NULL, rep("integer", times = 8))
  ) %>%
    dplyr::select(
      trial = V2,
      session = V3,
      block = V4,
      within_block_trial = V5,
      stimulus_excerpt_no = V6,
      condition = V7,
      coding_type_of_serial_order_of_conditions = V8,
      counter_for_stimuli = V9
    ) %>%
    # Append a column to design data frae to display the combination of condition
    # and stimulus excerpt number.
    dplyr::mutate(
      stimulus_id = sprintf(
        "%s%02i", condition, stimulus_excerpt_no
      )
    )
}
design_a <- read_design("data-raw/designA_three_sessions.txt")
design_b <- read_design("data-raw/designB_three_sessions.txt")
design_c <- read_design("data-raw/designC_three_sessions.txt")
design_d <- read_design("data-raw/designD_three_sessions.txt")

# generate vector per design and session
for (l in letters[1:4]) {
  for (s in 1:3) {
    value <-
      get(paste0("design_", l)) %>%
      dplyr::filter(session == s) %>%
      dplyr::pull("stimulus_id")
    assign(
      x = paste0(l, s, 3),
      value = value,
      envir = .GlobalEnv
    )
  }
}
rm(value)
# generate list of designs
experiment4_designs <-
  purrr::map(
    stats::setNames(
      letters[1:4],
      stringr::str_c("design_", letters[1:4])
    ),
    function(l) {
      res <-
        list(
          df = get(stringr::str_c("design_", l))
        )
      for (s in 1:3) {
        res[[stringr::str_c(l, s)]] <- get(stringr::str_c(l, s, 3))
      }
      res
    }
  )
rm(l, s)

usethis::use_data(
  experiment4_designs,
  overwrite = TRUE,
  internal = FALSE
)
