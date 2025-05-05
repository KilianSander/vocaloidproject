#' Individual emotional baseline
#'
#' This function creates a page examining the emotional baseline of
#' a participant.
#'
#'
emotional_baseline_page <- function(label = "emotional_baseline",
                                    dict = vocaloidproject::vocaloidproject_dict,
                                    default_lang = "de_f") {
  stopifnot(psyquest:::is.scalar.character(label),
            is.character(default_lang))

  psychTestR::new_timeline(
    psychTestR::page(
      ui = shiny::tags$div(
        GAR:::tagify(psychTestR::i18n("EMOBASE_PROMPT")),
        GAR::make_ui_radiobutton_matrix(
          polarity = "unipolar",
          items = sapply(paste0("EMOBASE_ITEM", 1:6), psychTestR::i18n, simplify = TRUE, USE.NAMES = TRUE),
          scale_labels = sapply(paste0("EMOBASE_CHOICE", 1:5), psychTestR::i18n, simplify = TRUE, USE.NAMES = TRUE),
          choices = 1:5,
          reduce_labels = FALSE,
          anchors = FALSE,
          header = "double",
          sublabel_type = "directed",
          header_style = NULL,
          trigger_button_text = psychTestR::i18n("CONTINUE"),
          hide = FALSE,
          id = "response_ui"
        )
      ),
      admin_ui = NULL,
      label = label,
      final = FALSE,
      get_answer = function(input, ...) {
        values <-shiny::reactiveValuesToList(input)$radio_matrix
        answer <- purrr::map(values, function(x){
          if(!is.null(x[[1]])) as.numeric(x[[1]])
          else NA
        }) %>% unlist()
        names(answer) <- sprintf("%s.q%d", label, 1:6)
        answer
      },
      save_answer = TRUE,
      validate = function(answer, ...) {
        valid <- TRUE
        if (any(is.na(answer))) {
          valid <- failed_validation_message
        }
        valid
      },
      on_complete = NULL,
      next_elt = TRUE
      #
    # GAR::make_ui_radiobutton_matrix(
    #   label = label,
    #   polarity = "unipolar",
    #   items = purrr::map_vec(paste0("EMOBASE_ITEM", 1:6), psychTestR::i18n),
    #   choices = as.character(1:5),
    #   labels = purrr::map(paste0("EMOBASE_CHOICE", 1:5), psychTestR::i18n),
    #   instruction = psychTestR::i18n("EMOBASE_PROMPT"),
    #   anchors = FALSE,
    #   header = "double",
    #   reduce_labels = TRUE,
    #   sublabel_type = "directed",
    #   trigger_button_text = psychTestR::i18n("CONTINUE"),
    #   allow_na = FALSE,
    #   failed_validation_message = psychTestR::i18n("ANSWER_NEEDED"),
    #   save_answer = TRUE,
    #   hide_response_ui = FALSE,
    #   random_order = FALSE
    ),
    dict = dict,
    default_lang = default_lang
  )
}

# polarity = "bipolar",
# items = purrr::map_vec(paste0("EMOBASE_ITEM", 1:6), psychTestR::i18n),

# header = "double",
# trigger_button_text = psychTestR::i18n("NEXT"),
# allow_na = FALSE,
