
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

translate <- function(words,
                      target_lang = "DE") {
  deeplr::translate2(
    text = words,
    target_lang = target_lang,
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

lyrics$LYRICS_en_deepL <-
  purrr::map_chr(
    lyrics$`LYRICS (original language)`,
    function(words) {
      translate(
        words = words,
        target_lang = "EN"
      )
    }
  )

save(
  lyrics,
  file = "data-raw/lyrics+deepL.Rda"
)
write.csv(
  lyrics,
  file = "data-raw/lyrics+deepL.csv",
  fileEncoding = "UTF-8",
  na = "",
  row.names = FALSE
)
