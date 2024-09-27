## code to prepare `vocaloidproject_dict` dataset goes here
educational_degrees <-
  tibble::tribble(
    ~de, ~ja,
    "Berufsausbildung/Fachhochschule/Fachoberschule", "準学士/高度専門士/専門士",
    "Abitur (HS)", "高卒",
    "Fachhochschuldiplom", "短期大学士",
    "Bachelor (Uni)", "学士",
    "Master", "修士",
    "Promotion/PhD", "博士"
  )

usethis::use_data(vocaloidproject_dict, overwrite = TRUE)
