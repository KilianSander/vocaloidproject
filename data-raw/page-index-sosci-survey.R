## get php array index for 45 stimulus pages and 6 break pages in soscisurvey
library(tidyverse)
df <-
  data.frame(
    index = 0:50,
    page = c(
      "block1",
      str_c("stimulus_", 1:8),
      "block2",
      str_c("stimulus_", 9:15),
      "block3",
      str_c("stimulus_", 16:23),
      "block4",
      str_c("stimulus_", 24:30),
      "block5",
      str_c("stimulus_", 31:38),
      "block6",
      str_c("stimulus_", 39:45)
    )
  )
df %>%
  filter(str_detect(page, "block"))
