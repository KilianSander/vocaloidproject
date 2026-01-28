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

# see https://github.com/klausfrieler/psyquest/blob/a14d0ab7cc75e82e394f4149868d04d829eae2cb/R/utils.R#L325C1-L339C2
problems_info <- function(researcher_email) {
  stopifnot(is.scalar.character(researcher_email))
  langs <- c("de", "de_f", "en", "ja")
  problems_info_html <-
    purrr::map(
      langs,
      function(lang) {
        span <-
          shiny::tags$span(
            vocaloidproject::vocaloidproject_dict$translate("PROBLEMS_INFO_1", lang),
            shiny::tags$br(),
            vocaloidproject::vocaloidproject_dict$translate("PROBLEMS_INFO_2", lang),
            shiny::tags$a(href = paste0("mailto:", researcher_email), researcher_email),
            vocaloidproject::vocaloidproject_dict$translate("PROBLEMS_INFO_3", lang),
            shiny::tags$br()
          )
      }
    ) %>%
    setNames(langs)
  problems_info_html
}

problems_info_language_mails <- function(named_researcher_emails = NULL) {
  stopifnot(
    is.null(named_researcher_emails) | is.character(named_researcher_emails)
  )
  if (is.null(named_researcher_emails)) {
    ret <- "default"
  } else {
    if (is.null(names(named_researcher_emails))) {
      stop(
        paste0(
          "`named_researcher_emails` has to be a named vector (or `NULL`). ",
          "Names have to match the codes of the languages supported by the ",
          "questionnaire."
        )
      )
    }
    ret <-
      purrr::imap(
        named_researcher_emails,
        function(x, idx) {
          shiny::tags$span(
            vocaloidproject::vocaloidproject_dict$translate("PROBLEMS_INFO_1", idx),
            shiny::tags$br(),
            vocaloidproject::vocaloidproject_dict$translate("PROBLEMS_INFO_2", idx),
            shiny::tags$a(href = paste0("mailto:", x), x),
            vocaloidproject::vocaloidproject_dict$translate("PROBLEMS_INFO_3", idx),
            shiny::tags$br()
          )
        }
      )
  }
  ret
}

# helper functions for monitor app
# https://github.com/klausfrieler/dislikes_monitor/blob/45fb47c0d0055179d757794dfae584316ab02736/analysis.R#L246C1-L270C2
remove_doublets <- function(results){
  # browser()
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
              dplyr::select(
                dplyr::all_of(c("p_id", "time_started", "complete"))
              )
          }
          if (is.null(x$session) || nrow(session_data) == 0) {
            session_data <- data.frame(p_id = NA)
          }
          deg <- parse_deg(x$DEG)
          edu <- parse_edu(x$EDU)
          voice_rating <- parse_voice_rating(x)
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
  ja_skills_key <-
    dplyr::filter(
      vocaloidproject::vocaloidproject_dict_df,
      stringr::str_detect(key, "JAPANESE_SKILLS_CHOICE")
    ) %>%
    dplyr::select(key, de_f) %>%
    tibble::deframe()
  edu_data %>%
    as.data.frame() %>%
    dplyr::mutate(
      educational_degree = ordered(
        educational_degree,
        levels = 1:6,
        labels = c(
          "Berufsausbildung/Fachhochschule/Fachoberschule",
          "Abitur", "Fachhochschuldiplom",
          "Bachelor", "Master", "PhD"
        )
      ),
      japanese_skills = ja_skills_key[
        paste0("JAPANESE_SKILLS_CHOICE", japanese_skills)
      ]
    )
}

parse_voice_rating <- function(x) {
  # browser()
  voice_rating_data <- x$voice_rating
  stimulus_order <-
    voice_rating_data[["voice_rating_stimulus_order"]] %>%
    paste0(collapse = ";")
  rating_scale <- ifelse(
    x$results$voice_rating_reversed,
    "human-artificial", "artificial-human"
  )
  ratings <-
    voice_rating_data[
      names(voice_rating_data) != "voice_rating_stimulus_order"
    ] %>%
    as.data.frame() %>%
    dplyr::select(
      order(colnames(.))
    )
  cbind(stimulus_order, rating_scale, ratings)
}

# see https://github.com/klausfrieler/dislikes_monitor/blob/45fb47c0d0055179d757794dfae584316ab02736/analysis.R#L351C1-L420C2
setup_voice_rating_workspace <- function(results = "data_raw",
                                         reload = FALSE,
                                         save_update = FALSE) {
  if (!file.exists("data/master.rds")) {
    master <- read_voice_rating_data(results)
    saveRDS(master, "data/master.rds")
  } else {
    if (reload) {
      master <- read_voice_rating_data(results)
      if (save_update) {
        saveRDS(master, "data/master.rds")
      }
    } else {
      master <- readRDS("data/master.rds")
    }
  }
  assign("master", master, globalenv())
  invisible(master)
}

p_id_to_seed <- function(p_id) {
  p_id %>%
    digest::sha1() %>%
    charToRaw() %>%
    as.integer() %>%
    sum()
}

sosci_api_import <- function(sosci_data_url) {
  stopifnot(is.scalar.character(sosci_data_url))
  ds <-
    read.delim(
      file = sosci_data_url,
      encoding = "UTF-8",
      fileEncoding = "UTF-8",
      sep = "\t",
      quote = "\"",
      dec = ".",
      as.is = TRUE,
      na.strings = ""
    )
  return(ds)
}

parse_HALT_selfreport_device <- function(halt_result) {
  if (!is.null(halt_result)) {
    data.frame(
      playback_device = halt_result[["device_selfreport"]][["answer"]][[1]]
    )
  } else {
    data.frame(
      playback_device = NA_character_
    )
  }

}

parse_emotional_baseline <- function(emo_base_result) {
  if (!is.null(emo_base_result)) {
    emo_baseline <-
      emo_base_result %>%
      as.data.frame() %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::everything(),
          as.numeric
        )
      )
  } else {
    emo_baseline <-
      data.frame(emotional_baseline.q1 = NA_real_)
  }
  return(emo_baseline)
}

parse_asa <- function(asa_result) {
  if(is.null(asa_result)) {
    data.frame(
      ASA.deification = NA_real_,
      SA.incarnation = NA_real_
    )
  } else {
    asa_result %>%
      as.data.frame()
  }
}

parse_bes <- function(bes_result) {
  if(is.null(bes_result)) {
    data.frame(
      BES.affective = NA_real_,
      BES.cognitive = NA_real_
    )
  } else {
    bes_result %>%
      as.data.frame()
  }
}
