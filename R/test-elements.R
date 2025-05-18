#' Create UI for Visual Analog Scale
#'
#' Creates HTML code for a visual analog scale slider.
#'
#' @param min_label (scalar character) Label for the left endpoint of the scale,
#' which corresponds to the minimum value of the slider.
#'
#' @param max_label (scalar character) Label for the right endpoint of the
#' scale, which corresponds to the maximum value of the slider.
#'
#' @param min_numeric (scalar numeric) Minimum value for the slider.
#'
#' @param max_numeric (scalar numeric) Maximum value for the slider.
#'
#' @param value (scalar numeric) Initial value of the slider.
#' Must lie between `min` and `max`.
#'
#' @param slider_width (scalar) Width of the slider. Must be valid for use as a
#' CSS unit of length.
#'
#' @inheritParams shiny::sliderInput
#'
#' @export
make_ui_vas <- function(min_label,
                        max_label,
                        min_numeric,
                        max_numeric,
                        value,
                        step,
                        round = FALSE,
                        ticks = FALSE,
                        animate = FALSE,
                        slider_width = "300px",
                        sep = ",",
                        pre = NULL, post = NULL) {
  stopifnot(psychTestR:::is.scalar.character(min_label),
            psychTestR:::is.scalar.character(max_label),
            psychTestR:::is.scalar.numeric(min_numeric),
            psychTestR:::is.scalar.numeric(max_numeric),
            psychTestR:::is.scalar.numeric(value),
            psychTestR:::is.scalar.numeric(step),
            psychTestR:::is.scalar(slider_width),
            value <= max_numeric,
            value >= min_numeric)

  slider_width <- shiny::validateCssUnit(slider_width)

  ui <- shiny::sliderInput(
    inputId = "slider",
    # https://stackoverflow.com/questions/40415471/sliderinput-max-min-text-labels
    label = shiny::tags$div(
      style = paste0('width:', slider_width,';'),
      shiny::tags$div(style='float:left;', min_label),
      shiny::tags$div(style='float:right;', max_label)
    ),
    min = min_numeric,
    max = max_numeric,
    value = value,
    step = step,
    round = round,
    ticks = ticks,
    animate = animate,
    width = slider_width,
    sep = sep,
    pre = pre,
    post = post,
    timeFormat = NULL,
    timezone = NULL,
    dragRange = FALSE
  )
}

#' New Visual Analog Scale Page
#'
#' Creates a page where the participant responds by using a slider like a
#' visual analog scale.
#'
#' @inheritParams make_ui_vas
#'
#' @inheritParams psychTestR::slider_page
#'
#' @export
vas_page <- function(label,
                     prompt,
                     min_label,
                     max_label,
                     min_numeric,
                     max_numeric,
                     value,
                     save_answer = TRUE,
                     button_text = "Next",
                     on_complete = NULL,
                     admin_ui = NULL,
                     step,
                     round = FALSE,
                     ticks = FALSE,
                     animate = FALSE,
                     slider_width = "300px",
                     sep = ",",
                     pre = NULL,
                     post = NULL) {
  stopifnot(psychTestR:::is.scalar.character(label))

  slider <- make_ui_vas(
    min_label = min_label,
    max_label = max_label,
    min_numeric = min_numeric,
    max_numeric = max_numeric,
    value = value,
    step = step,
    round = round,
    ticks = ticks,
    animate = animate,
    slider_width = slider_width,
    sep = sep, pre = pre, post = post
  )
  body <- shiny::div(
    psychTestR:::tagify(prompt),
    slider
  )
  ui <- shiny::div(
    body,
    psychTestR::trigger_button("next", button_text)
  )

  # test_options(display_options(css)) should take care of the following:
  # tags$head(tags$style(HTML('.irs-from, .irs-to, .irs-min, .irs-max, .irs-single {
  #           visibility: hidden !important;
  #   }')))

  get_answer <- function(input, ...) input$slider

  psychTestR::page(
    ui = ui,
    label = label,
    admin_ui = admin_ui,
    final = FALSE,
    get_answer = get_answer,
    save_answer = save_answer,
    on_complete = on_complete,
    next_elt = TRUE
  )
}
