#' Mobile devices landscape
#'
#' This function creates a page prompting participants to use mobile devices
#' in landscape mode.
#'
#' @param img_src `NULL` for no image and a URL (character scalar) to display
#' an image.
#'
#' @param img_width CSS unit (see [shiny::validateCssUnit()]) for the width
#' of the image.
#'
#' @param img_align (character scalar) sets the alignment of the image.
#'
#' @inheritParams emotional_baseline_page
#'
#' @export
use_landscape_page <- function(img_src = NULL,
                               img_width = "100%",
                               img_align = "center",
                               dict = vocaloidproject::vocaloidproject_dict,
                               default_lang = "de_f") {
  stopifnot(is.null(img_src) | is.scalar.character(img_src),
            is.scalar.character(img_align))
  img_width <- shiny::validateCssUnit(img_width)

  psychTestR::new_timeline(
    psychTestR::one_button_page(
      body = shiny::div(
        shiny::p(
          psychTestR::i18n("LANDSCAPE_PROMPT")
        ),
        if (!is.null(img_src)) {
          shiny::img(
            src = img_src,
            alt = psychTestR::i18n("LANDSCAPE_PROMPT"),
            width = img_width,
            align = img_align
          )
        }

      ),
      button_text = psychTestR::i18n("CONTINUE")
    ),
    dict = dict,
    default_lang = default_lang
  )
}
