
api_key <-
  readLines(
    con = "deepl-api-key",
    n = 1
  )

lyrics <-
  readr::read_csv2(
    file = "data-raw/20240923_excerpt.list.lyrics.csv"
  )
lyrics <-
  lyrics |>
  dplyr::filter(
    !is.na(`LYRICS (original language)`)
  ) |>
  dplyr::select(
    !dplyr::starts_with("...1")
  )

translate <- function(words) {
  deeplr::translate2(
    text = words,
    target_lang = "DE",
    source_lang = "JA",
    split_sentences = TRUE,
    preserve_formatting = FALSE,
    auth_key = api_key
  )
}

lyrics$LYRICS_de_deepL <-
  purrr::map_chr(
    lyrics$`LYRICS (original language)`,
    translate
  )
