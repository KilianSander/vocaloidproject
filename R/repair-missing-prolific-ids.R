extract_prolific_id <- function(psychtestr_session_data) {
  purrr::pluck(
    psychtestr_session_data,
    "passive", "url_params", "PROLIFIC_PID"
  )
}

extract_psychtestr_id <- function(psychtestr_session_data) {
  purrr::pluck(
    psychtestr_session_data,
    "passive", "p_id"
  )
}

id_lookup_table <- function(output_dir) {
  stopifnot(dir.exists(output_dir))
  session_files <-
    list.files(
      path = file.path(output_dir, "sessions"),
      pattern = "data.RDS",
      full.names = TRUE,
      recursive = TRUE
    )
  purrr::map(
    session_files,
    function(session_file) {
      tmp <- readRDS(session_file)
      prolific <- extract_prolific_id(tmp)
      data.frame(
        prolific_id = ifelse(is.null(prolific), NA, prolific),
        p_id = extract_psychtestr_id(tmp)
      )
    }
  ) %>%
    purrr::list_rbind()
}
