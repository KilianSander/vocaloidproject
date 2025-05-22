is.scalar <- function(x) {
  identical(length(x), 1L)
}

is.scalar.character <- function(x) {
  is.character(x) && is.scalar(x)
}

is.scalar.numeric <- function(x) {
  is.numeric(x) && is.scalar(x)
}

is.scalar.logical <- function(x) {
  is.logical(x) && is.scalar(x)
}

is.integerlike <- function(x) {
  all(round(x) == x)
}

is.scalar.integerlike <- function(x) {
  is.scalar(x) && is.integerlike(x)
}

tagify <- function(x) {
  stopifnot(is.character(x) || is(x, "shiny.tag"))
  if (is.character(x)) {
    stopifnot(is.scalar(x))
    shiny::p(x)
  } else x
}

# helper functions for monitor app
# https://github.com/klausfrieler/dislikes_monitor/blob/45fb47c0d0055179d757794dfae584316ab02736/analysis.R#L246C1-L270C2
remove_doublets <- function(results){
  #browser()
  diagnostic <-
    purrr::map(
      1:length(results),
      function(i){
        if(!"session" %in% names(results[[i]])){
          return(NULL)
        }
        tmp <- results[[i]]$session %>%
          tibble::as_tibble() %>%
          dplyr::select(p_id, current_time, complete) %>%
          dplyr::mutate(idx = i)
      }
    ) %>%
    purrr::list_rbind()
  incompletes <- which(!diagnostic$complete)
  p_ids <- unique(diagnostic$p_id)
  not_last <- diagnostic %>%
    dplyr::group_by(p_id) %>%
    dplyr::mutate(r = rank(current_time), last = (r == max(r))) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!last)
  p_ids_after <- unique(not_last$p_id)
  if(length(setdiff(p_ids_after, p_ids)) != 0){
    browser()
  }
  remove_idx <- union(incompletes, not_last %>% dplyr::pull(idx))
  message(sprintf("Removing %d doublets", length(remove_idx)))
  results[setdiff(1:length(results), remove_idx)]
}

read_voice_rating_data <- function(results_dir = results_dir) {
  # browser()
  results <- purrr::map(
    list.files(path = results_dir, pattern = "*.rds", full.names = TRUE),
    function(x) {
      readRDS(x) %>% as.list()
    }
  )
  results <- remove_doublets(results)
  if (length(results) > 0) {
    ret <-
      results %>%
      purrr::map(
        function(x) {
          x <- as.list(x)
          if (!is.null(x$session)) {
            session_data <-
              x$session %>%
              as.data.frame() %>%
              dplyr::select(p_id, time_started, complete)
          }
          if (is.null(x$session) || nrow(session_data) == 0) {
            session_data <- data.frame(p_id = NA)
          }
          deg <- parse_deg(x$DEG)
          edu <- parse_edu(x$EDU)
          voice_rating <- parse_voice_rating(x$voice_rating)
          person_data <-
            dplyr::bind_cols(
              session_data,
              deg, edu,
              voice_rating
            )
          return(person_data)
        }
      ) %>%
      purrr::list_rbind() %>%
      dplyr::arrange(time_started)
  } else {
    ret <- data.frame(
      p_id = character(0L)
    )
  }
  return(ret)
}

parse_deg <- function(deg_data) {
  deg_data[c("Age", "Gender")] %>%
    as.data.frame() %>%
    dplyr::mutate(
      Gender = Gender %>%
        dplyr::case_match(
          1 ~ "female",
          2 ~ "male",
          3 ~ "other",
          4 ~ "rather not say"
        ),
      Age_months = Age,
      Age_years = Age %/% 12,
      .keep = "unused"
    )
}

parse_edu <- function(edu_data) {
  edu_data %>%
    as.data.frame() %>%
    dplyr::mutate(
      educational_degree = ordered(educational_degree)
    )
}

parse_voice_rating <- function(voice_rating_data) {
  stimulus_order <-
    voice_rating_data[["voice_rating_stimulus_order"]] %>%
    paste0(collapse = ";")
  ratings <-
    voice_rating_data[
      names(voice_rating_data) != "voice_rating_stimulus_order"
    ] %>%
    as.data.frame()
  cbind(stimulus_order, ratings)
}

# see https://github.com/klausfrieler/dislikes_monitor/blob/45fb47c0d0055179d757794dfae584316ab02736/analysis.R#L351C1-L420C2
setup_voice_rating_workspace <- function(results = "data_raw",
                                         reload = FALSE) {
  if (reload || !file.exists("data/master.rds")) {
    master <- read_voice_rating_data(results)
    saveRDS(master, "data/master.rds")
  } else {
    master <- readRDS("data/master.rds")
  }
  assign("master", master, globalenv())
}
