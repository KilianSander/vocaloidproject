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
#' @param hide_numeric_values (logical scalar) Whether or not to hide the
#' numeric values in the UI, that is, `min_numeric`, `max_numeric`, and `value`.
#'
#' @param fill_bar (logical scalar) `r lifecycle::badge("experimental")`
#' Whether or not to fill the bar from
#' the minimum to the position of the slider.
#' Currently only works for the first occurrence of a slider.
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
                        hide_numeric_values = TRUE,
                        fill_bar = FALSE,
                        round = FALSE,
                        ticks = FALSE,
                        animate = FALSE,
                        slider_width = "300px",
                        sep = ",",
                        pre = NULL, post = NULL) {
  stopifnot(is.scalar.character(min_label),
            is.scalar.character(max_label),
            is.scalar.numeric(min_numeric),
            is.scalar.numeric(max_numeric),
            is.scalar.numeric(value),
            is.scalar.numeric(step),
            is.scalar(slider_width),
            value <= max_numeric,
            value >= min_numeric,
            is.scalar.logical(hide_numeric_values),
            is.scalar.logical(fill_bar))

  slider_width <- shiny::validateCssUnit(slider_width)

  ui <- shiny::sliderInput(
    inputId = "slider",
    # https://stackoverflow.com/questions/40415471/sliderinput-max-min-text-labels
    label = shiny::tags$div(
      style = paste0('width:', slider_width,';'),
      shiny::tags$div(
        style='float:left;',
        shiny::div(min_label, style='font-size:120%;')
      ),
      shiny::tags$div(
        style='float:right;',
        shiny::div(max_label, style='font-size:120%;')
      )
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

  if (hide_numeric_values) {
    ui <- shiny::div(
      # see https://stackoverflow.com/questions/35251788/hide-values-of-sliderinput-in-shiny
      shiny::tags$head(
        shiny::tags$style(
          shiny::HTML(
            paste0(
              '.irs-from, .irs-to, ',
              '.irs-min, .irs-max, ',
              '.irs-single {visibility: hidden !important;}'
            )
          )
        )
      ),
      ui
    )
  }
  if (!fill_bar) {
    ui <- shiny::div(
      shiny::tags$head(
        shiny::tags$style(
          shiny::HTML(
            paste0(
              # thanks to @violapsch !
              ".irs-bar",
              "{background: transparent!important; ",
              "border-color: transparent!important;}"
            )
          )
        )
      ),
      # shinyWidgets::chooseSliderSkin(color = "transparent"),
      # shiny::singleton(
      #   shiny::tags$head(
      #     shiny::tags$style(
      #       shiny::HTML(
      #         paste0(".irs-bar-edge, .irs-bar, .irs-single, .irs-from, ",
      #                ".irs-to {background: transparent border: transparent !important;}"
      #         )
      #       )
      #     )
      #   )
      # ),
      ui
    )
  }

  ui
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
#' @param reverse (logical scalar) Whether or not the slider should be reversed.
#' Usually (i.e., `reversed = FALSE`) `min_*` is left and `max_*` is right.
#' When reversed (i.e., `reversed = TRUE`),
#' `min_*` is right and `max_*` is left.
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
                     hide_numeric_values = TRUE,
                     fill_bar = TRUE,
                     reverse = FALSE,
                     round = FALSE,
                     ticks = FALSE,
                     animate = FALSE,
                     slider_width = "300px",
                     sep = ",",
                     pre = NULL,
                     post = NULL) {
  stopifnot(is.scalar.character(label),
            is.scalar.logical(reverse))

  slider <- make_ui_vas(
    min_label = ifelse(reverse, max_label, min_label),
    max_label = ifelse(reverse, min_label, max_label),
    min_numeric = min_numeric,
    max_numeric = max_numeric,
    value = value,
    step = step,
    hide_numeric_values = hide_numeric_values,
    fill_bar = fill_bar,
    round = round,
    ticks = ticks,
    animate = animate,
    slider_width = slider_width,
    sep = sep, pre = pre, post = post
  )
  body <- shiny::div(
    tagify(prompt),
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

  get_answer <- function(input, ...) {
    answer <- input$slider
    if(reverse) {
      answer <- max_numeric - answer + min_numeric
    }
    answer
  }

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

# see https://github.com/pmcharrison/psychTestR/blob/cf42f20d3c156ebb42721af1310849ff8735ea1d/R/test-elements.R#L847C1-L857C2
media.js <- list(
  media_not_played = "var media_played = false;",
  media_played = "media_played = true;",
  play_media = "document.getElementById('media').play();",
  show_media_btn = paste0("if (!media_played) ",
                          "{document.getElementById('btn_play_media')",
                          ".style.visibility='inherit'};"),
  hide_media_btn = paste0("document.getElementById('btn_play_media')",
                          ".style.visibility='hidden';"),
  show_responses = "document.getElementById('response_ui').style.visibility = 'inherit';"
)

# see https://github.com/pmcharrison/psychTestR/blob/cf42f20d3c156ebb42721af1310849ff8735ea1d/R/test-elements.R#L859C1-L863C53
media_mobile_play_button <- function(btn_play_prompt) shiny::tags$p(
  shiny::tags$strong(btn_play_prompt,
                     id = "btn_play_media",
                     style = "visibility: hidden",
                     onclick = media.js$play_media))

#' New Audio Visual Analog Scale Page
#'
#' Creates a page with an audio prompt where the participant responds by
#' using a slider like a visual analog scale.
#'
#' @inheritParams vas_page
#'
#' @inheritParams psychTestR::audio_NAFC_page
#'
#' @export
audio_vas_page <- function(label,
                           prompt,
                           min_label,
                           max_label,
                           min_numeric,
                           max_numeric,
                           value,
                           url,
                           type = tools::file_ext(url),
                           wait = TRUE,
                           loop = FALSE,
                           btn_play_prompt = if (!show_controls) "Click here to play",
                           show_controls = TRUE,
                           allow_download = FALSE,
                           autoplay = "autoplay",
                           save_answer = TRUE,
                           button_text = "Next",
                           on_complete = NULL,
                           admin_ui = NULL,
                           step,
                           hide_numeric_values = TRUE,
                           fill_bar = TRUE,
                           reverse = FALSE,
                           round = FALSE,
                           ticks = FALSE,
                           animate = FALSE,
                           slider_width = "300px",
                           sep = ",",
                           pre = NULL,
                           post = NULL) {
  stopifnot(is.scalar.character(label),
            is.scalar.character(url),
            is.scalar.logical(loop),
            is.scalar.logical(wait))
  # see https://github.com/pmcharrison/psychTestR/blob/cf42f20d3c156ebb42721af1310849ff8735ea1d/R/test-elements.R#L928C1-L942C50
  audio_ui <- shiny::tags$div(
    shiny::tags$audio(
      shiny::tags$head(shiny::tags$script(shiny::HTML(media.js$media_not_played))),
      shiny::tags$source(src = url, type = paste0("audio/", type)),
      id = "media",
      preload = "auto",
      autoplay = if(nchar(autoplay) > 0) "autoplay",
      loop = if (loop) "loop",
      oncanplaythrough = media.js$show_media_btn,
      onplay = paste0(media.js$media_played, media.js$hide_media_btn),
      onended = if (wait) media.js$show_responses else "null",
      controls = if (show_controls) "controls",
      controlsList = if (!allow_download) "nodownload"
    ),
    media_mobile_play_button(btn_play_prompt)
  )
  prompt2 <- shiny::div(
    tagify(prompt),
    audio_ui
  )

  vas_page(
    label = label,
    prompt = prompt2,
    min_label = min_label,
    max_label = max_label,
    min_numeric = min_numeric,
    max_numeric = max_numeric,
    value = value,
    save_answer = save_answer,
    button_text = button_text,
    on_complete = on_complete,
    admin_ui = admin_ui,
    hide_numeric_values = hide_numeric_values,
    fill_bar = fill_bar,
    reverse = reverse,
    step = step,
    round = round,
    ticks = ticks,
    animate = animate,
    slider_width = slider_width,
    sep = sep,
    pre = pre,
    post = post
  )
}

#' Battery of Audio Visual Analog Scale Pages
#'
#' @inheritParams audio_vas_page
#'
#' @param battery_label (character scalar) Name for the results section of the
#' battery. Individual page labels (see [psychTestR::page()]) are generated
#' from `battery_label` and `stimulus_prefix_pattern`.
#'
#' @param num_stimuli (integer-like scalar) Number of stimuli / pages.
#'
#' @param stimulus_prefix_pattern (character scalar) File name pattern of the
#' stimuli (internally used in `base::sprintf()`).
#' For example, `num_stimuli = 4`, `stimulus_prefix_pattern = "s%03d"`, and
#' `type = "mp3"` results in the file names `s001.mp3`, `s002.mp3`,
#' `s003.mp3`, and `s004.mp3`.
#'
#' @param base_url (character scalar) URL without the stimulus file names.
#'
#' @param randomise_at_runtime (logical scalar) Whether or not to randomise the
#' order of the VAS pages.
#'
#' @export
audio_vas_page_battery <- function(battery_label,
                                   prompt,
                                   min_label,
                                   max_label,
                                   min_numeric,
                                   max_numeric,
                                   value,
                                   num_stimuli,
                                   stimulus_prefix_pattern = "s%03d",
                                   base_url,
                                   randomise_at_runtime = FALSE,
                                   type = "mp3",
                                   wait = TRUE,
                                   loop = FALSE,
                                   btn_play_prompt = if (!show_controls) "Click here to play",
                                   show_controls = TRUE,
                                   allow_download = FALSE,
                                   autoplay = "autoplay",
                                   save_answer = TRUE,
                                   button_text = "Next",
                                   on_complete = NULL,
                                   admin_ui = NULL,
                                   step,
                                   hide_numeric_values = TRUE,
                                   fill_bar = TRUE,
                                   reverse = FALSE,
                                   round = FALSE,
                                   ticks = FALSE,
                                   animate = FALSE,
                                   slider_width = "300px",
                                   sep = ",",
                                   pre = NULL,
                                   post = NULL) {
  stopifnot(is.scalar.character(battery_label),
            is.scalar.integerlike(num_stimuli),
            num_stimuli > 0,
            is.scalar.character(stimulus_prefix_pattern),
            is.scalar.character(base_url),
            is.scalar.logical(randomise_at_runtime))
  # see https://github.com/klausfrieler/GAR/blob/09f69e3b6707dd13baa21ee996a84fda3eb9da72/R/GAR.R#L70C1-L176C2
  audio_vas_pages <-
    lapply(
      1:num_stimuli,
      function(n) {
        page_label <- sprintf(
          paste0("%s_", stimulus_prefix_pattern),
          battery_label, n
        )
        stimulus_url <- file.path(
          base_url,
          sprintf(
            "%s.%s",
            sprintf(stimulus_prefix_pattern, n),
            type
          )
        )
        psychTestR::join(
          audio_vas_page(
            label = page_label,
            prompt = prompt,
            min_label = min_label,
            max_label = max_label,
            min_numeric = min_numeric,
            max_numeric = max_numeric,
            value = value,
            url = stimulus_url,
            type = ,
            wait = wait,
            loop = loop,
            btn_play_prompt = btn_play_prompt,
            show_controls = show_controls,
            allow_download = allow_download,
            autoplay = autoplay,
            save_answer = save_answer,
            button_text = button_text,
            on_complete = on_complete,
            admin_ui = admin_ui,
            step = step,
            hide_numeric_values = hide_numeric_values,
            fill_bar = fill_bar,
            reverse = reverse,
            round = round,
            ticks = ticks,
            animate = animate,
            slider_width = slider_width,
            sep = sep, pre = pre, post = post
          ),
          psychTestR::elt_save_results_to_disk(complete = FALSE)
        )
      }
    )

  # see https://github.com/klausfrieler/GAR/blob/09f69e3b6707dd13baa21ee996a84fda3eb9da72/R/GAR.R#L139C1-L173C6
  save_stimuli <- function(label) {
    function(order, state, ...) {
      stimuli <- sprintf(stimulus_prefix_pattern, 1:num_stimuli)[order]
      message(
        sprintf("Saving stimulus order for %s (length: %d): %s",
                label, length(order),
                paste(stimuli, collapse = ", "))
      )
      psychTestR::save_result(state, label, stimuli)
    }
  }

  if (randomise_at_runtime) {
    psychTestR::join(
      psychTestR::begin_module(label = battery_label),
      psychTestR::randomise_at_run_time(
        label = battery_label,
        logic = audio_vas_pages,
        save_order = save_stimuli(sprintf("%s_stimulus_order", battery_label))
      ),
      # psychTestR::elt_save_results_to_disk(complete = TRUE),
      psychTestR::end_module()
    )
  } else {
    psychTestR::join(
      psychTestR::begin_module(label = battery_label),
      psychTestR::order_at_run_time(
        label = battery_label,
        logic = audio_vas_pages,
        get_order = function(...) 1:num_stimuli,
        save_order = save_stimuli(sprintf("%s_stimulus_order", battery_label))
      ),
      # psychTestR::elt_save_results_to_disk(complete = TRUE),
      psychTestR::end_module()
    )
  }
}
