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
                        pre = NULL, post = NULL,
                        timeFormat = NULL,
                        timezone = NULL, dragRange = TRUE) {
  stopifnot(psychTestR:::is.scalar.character(min_label),
            psychTestR:::is.scalar.character(max_label),
            psychTestR:::is.scalar.numeric(min_numeric),
            psychTestR:::is.scalar.numeric(max_numeric),
            psychTestR:::is.scalar.numeric(value),
            psychTestR:::is.scalar.numeric(step),
            psychTestR:::is.scalar(slider_width))
  slider_width <- shiny::validateCssUnit(slider_width)

  ui <- shiny::sliderInput(
    inputId = "slider",
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
    timeFormat = timeFormat,
    timezone = timezone,
    dragRange = dragRange
  )
  ui
}
