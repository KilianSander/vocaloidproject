#' Individual emotional baseline (single page)
#'
#' `r lifecycle::badge("experimental")`
#' This function creates a page examining the emotional baseline of
#' a participant.
#'
#' @param dict internationalization dictionary.
#'
#' @param label Label for the current page.
#'
#' @param vertical (logical scalar) If `TRUE`, radio buttons are created
#' vertically below each of the items.
#' If `FALSE`, a radio button matrix will be used.
#'
#' @param default_lang Default language for the page.
#'
#' @export
emotional_baseline_page <- function(label = "emotional_baseline",
                                    dict = vocaloidproject::vocaloidproject_dict,
                                    vertical = TRUE,
                                    default_lang = "de_f") {
  stopifnot(is.scalar.character(label),
            is.scalar.logical(vertical),
            is.character(default_lang))

  # failed_validation_message <- psychTestR::i18n("ANSWER_NEEDED")

  if (vertical) {
    emo_base_page <-
      psychTestR::new_timeline(
        psychTestR::page(
          ui = shiny::tags$div(
            tagify(psychTestR::i18n("EMOBASE_PROMPT")),
            GAR::make_ui_radiobutton_multi_NAFC(
              label = label,
              items = sapply(paste0("EMOBASE_ITEM", 1:6), psychTestR::i18n, simplify = TRUE, USE.NAMES = FALSE),
              choices = as.character(1:5),
              labels = sapply(paste0("EMOBASE_CHOICE", 1:5), psychTestR::i18n, simplify = TRUE, USE.NAMES = FALSE),
              trigger_button_text = psychTestR::i18n("CONTINUE"),
              hide = FALSE,
              id = "response_ui"
            )
          ),
          label = label,
          final = FALSE,
          get_answer = function(input, ...) {
            values <-
              shiny::reactiveValuesToList(input)[paste(label, 1:6, sep = "_")]
            answer <- purrr::map(
              values,
              function(x) {
                if(!is.null(x[[1]])) {
                  as.numeric(x[[1]])
                } else NA
            }) %>% unlist()
            names(answer) <- sprintf("%s.q%d", label, 1:6)
            answer
          },
          save_answer = TRUE,
          validate = function(answer, ...) {
            valid <- TRUE
            if (any(is.na(answer))) {
              valid <- psychTestR::i18n("ANSWER_NEEDED")
            }
            valid
          },
          next_elt = TRUE,
          on_complete = NULL
        ),
        dict = dict,
        default_lang = default_lang
      )
  } else {
    emo_base_page <-
      psychTestR::new_timeline(
        psychTestR::page(
          ui = shiny::tags$div(
            tagify(psychTestR::i18n("EMOBASE_PROMPT")),
            GAR::make_ui_radiobutton_matrix(
              polarity = "unipolar",
              items = sapply(paste0("EMOBASE_ITEM", 1:6), psychTestR::i18n, simplify = TRUE, USE.NAMES = TRUE),
              scale_labels = sapply(paste0("EMOBASE_MATRIX_CHOICE", 1:5), psychTestR::i18n, simplify = TRUE, USE.NAMES = TRUE),
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
              valid <- psychTestR::i18n("ANSWER_NEEDED")
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
  emo_base_page
}

# polarity = "bipolar",
# items = purrr::map_vec(paste0("EMOBASE_ITEM", 1:6), psychTestR::i18n),

# header = "double",
# trigger_button_text = psychTestR::i18n("NEXT"),
# allow_na = FALSE,


# GAR::make_ui_radiobutton_multi_NAFC(
#   label = "test",
#   items = paste0("Item ", 1:6),
#   choices = as.character(1:4),
#   labels = paste("Response", 1:4)
# )

#' Individual emotional baseline (multi page)
#'
#' `r lifecycle::badge("experimental")`
#' This function creates multiple pages examining the emotional baseline of
#' a participant.
#'
#' @inheritParams emotional_baseline_page
#'
#' @param vertical (logical scalar) Whether or not to arrange the
#' response buttons vertically.
#'
#' @export
emotional_baseline_single_pages <- function(label = "emotional_baseline",
                                            dict = vocaloidproject::vocaloidproject_dict,
                                            vertical = FALSE,
                                            default_lang = "de_f") {
  stopifnot(is.scalar.character(label),
            is.scalar.logical(vertical),
            is.character(default_lang))
  psychTestR::new_timeline(
    psychTestR::join(
      purrr::map(
        1:6,
        function(x) {
          psychTestR::NAFC_page(
            label = sprintf("%s.q%d", label, x),
            prompt = psychTestR::i18n(paste0("EMOBASE_ITEMPROMPT", x)),
            choices = as.character(1:5),
            labels = sapply(paste0("EMOBASE_CHOICE", 1:5), psychTestR::i18n, simplify = TRUE, USE.NAMES = FALSE),
            save_answer = TRUE,
            arrange_vertically = vertical,
            hide_response_ui = FALSE,
            on_complete = NULL,
            admin_ui = NULL,
            button_style = "min-width: 100px; min-height: 39 px;"
          )
        }
      )
    ),
    dict = dict,
    default_lang = default_lang
  )
}
