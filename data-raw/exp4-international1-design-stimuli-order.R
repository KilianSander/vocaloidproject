# Design Stimuli Presentation Order for Experiment 4 (International 1) ---------
#
# `data-raw/createDesign.py` creates four design files:
# `designA.txt`, `designB.txt`, `designC.txt`, and `designD.txt`.
# For details see Elke's documentation in `data-raw/createDesign.py`.
#
## Read design files -----------------------------------------------------------
require(dplyr)
read_design <- function(file) {
  read.table(
    file = file,
    header = FALSE,
    sep = " ",
    colClasses = c(NULL, rep("integer", times = 8))
  ) %>%
    select(
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
    mutate(
      stimulus_id = sprintf(
        "%s%02i", condition, stimulus_excerpt_no
      )
    )
}
design_a <- read_design("data-raw/designA.txt")
design_b <- read_design("data-raw/designB.txt")
design_c <- read_design("data-raw/designC.txt")
design_d <- read_design("data-raw/designD.txt")
## create vectors for each session and design ----------------------------------
for (l in letters[1:4]) {
  for (s in 1:2) {
    value <-
      get(paste0("design_", l)) %>%
      filter(session == s) %>%
      pull("stimulus_id") %>%
    # repeat each stimulus_id to prepare the check page (see next step)
      rep(each = 2)
    # append 'check' to every second stimulus_id for the SoSci check page
    value[(1:(length(value)/2))*2] <-
      paste0(
        value[(1:(length(value)/2))*2],
        "check"
      )
    # add the break pages
    value <- c(
      "block1", value[1:20], "block2", value[21:40], "block3", value[41:60],
      "block4", value[61:80], "block5", value[81:100], "block6", value[101:120]
    )
    assign(
      x = paste0(l, s),
      value = value,
      envir = .GlobalEnv
    )
  }
}
## Output for SoSci Survey -----------------------------------------------------
# Generate a php array from the vectors
output_for_sosci <-
  "data-raw/exp4-international1-design-stimuli-order-php-arrays.txt"
db_for_sosci <-
  "data-raw/exp4-international1-design-stimuli-order-db.csv"
file.create(output_for_sosci)
file.create(db_for_sosci)
for (l in letters[1:4]) {
  for (s in 1:2) {
    paste0(
      "$", l, s, " = array(",
      paste0("'", get(paste0(l, s)), "'", collapse = ", "),
      ");\n"
    ) %>%
      cat(
        file = output_for_sosci,
        append = TRUE
      )
    for (lang in c("ger", "jpn")) {
      cat(
        paste0(
          l, s, ",", lang, ",",
          paste0(get(paste0(l, s)), collapse = ","),
          "\n"
        ),
        file = db_for_sosci,
        append = TRUE
      )
    }
  }
}
# Import data-raw/exp4-international1-design-stimuli-order-db.csv to the
# SoSci Survey data base for content

# Create SoSci Survey data base entries for the 3-session version of
# `international1`, that is, `experiment4()`.
design_a$session_of_3 <- rep(1:3, each = 40)
design_b$session_of_3 <- rep(1:3, each = 40)
design_c$session_of_3 <- rep(1:3, each = 40)
design_d$session_of_3 <- rep(1:3, each = 40)
for (l in letters[1:4]) {
  for (s in 1:3) {
    value <-
      get(paste0("design_", l)) %>%
      filter(session_of_3 == s) %>%
      pull("stimulus_id") %>%
      # repeat each stimulus_id to prepare the check page (see next step)
      rep(each = 2)
    # append 'check' to every second stimulus_id for the SoSci check page
    value[(1:(length(value)/2))*2] <-
      paste0(
        value[(1:(length(value)/2))*2],
        "check"
      )
    # add the break pages
    value <- c(
      "block1", value[1:20],
      "block2", value[21:40],
      "block3", value[41:60],
      "block4", value[61:80]
    )
    # assign to global environment
    assign(
      x = paste0(l, s, 3),
      value = value,
      envir = .GlobalEnv
    )
    for (lang in c("ger", "jpn")) {
      cat(
        paste0(
          l, s, "3,", lang, ",",
          paste0(get(paste0(l, s, 3)), collapse = ","),
          "\n"
        ),
        file = "data-raw/exp4-international1-design-stimuli-order-db.csv",
        append = TRUE
      )
    }
  }
}
