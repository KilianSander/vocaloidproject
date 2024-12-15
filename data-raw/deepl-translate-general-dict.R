api_key <-
  readLines(
    con = "deepl-api-key",
    n = 1
  )

to_translate <-
  readr::read_csv2(
    file = "https://raw.githubusercontent.com/klausfrieler/psyquest/refs/heads/master/data_raw/dicts/general_dict.csv"
  ) |>
  dplyr::select(
    key,
    tidyselect::any_of(c("de", "de_f", "en"))
  ) |>
  dplyr::filter(
    !duplicated(key)
  )

translate <- function(words,
                      source_lang = "DE",
                      target_lang = "JA") {
  deeplr::translate2(
    text = words,
    target_lang = target_lang,
    source_lang = source_lang,
    split_sentences = TRUE,
    preserve_formatting = FALSE,
    auth_key = api_key
  )
}

deepl_translated <-
  tibble(
    key = to_translate$key,
    ja_de = to_translate$de %>%
      map_vec(translate),
    ja_de_f = to_translate$de_f %>%
      map_vec(translate),
    ja_en = to_translate$en %>%
      map_vec(
        function(x) {
          translate(
            words = x,
            source_lang = "EN"
          )
        }
      )
  )

write.csv(
  deepl_translated,
  file = "data-raw/general-dict-ja-by-deepl.csv",
  fileEncoding = "UTF-8",
  row.names = FALSE,
  na = ""
)
