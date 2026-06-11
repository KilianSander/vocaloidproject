#' Japanese variant of DEG Age and Gender subscales
#'
#' [psyquest::DEG()] provides self-report pages for age and gender.
#' The `ja_DEG()` function provides an age page that asks for year and month
#' of birth (instead of month and year) better reflecting the Japanese date
#' format.
#' The gender self-report page is identical to the one in [psyquest::DEG()].
#' Use `ja_DEG()` and [psyquest::DEG()] in conjunction with
#' [psychTestR::conditional()] to display the adequate date format based on
#' the language.
#'
#' @param dict Dictionary used for internationalization.
#'
#' @inheritParams psyquest::DEG
#'
#' @export
ja_DEG <- function(label = "DEG",
                   dict = vocaloidproject::vocaloidproject_dict,
                   subscales = c(),
                   languages = "ja",
                   year_range = c(1930, 2013),
                   ...) {
  stopifnot(is.scalar.character(label),
            length(year_range) == 2)
  questionnaire_id <- "DEG"
  if (is.null(subscales) || length(subscales) == 0) {
    subscales <- c("Age", "Gender")
  } else {
    subscales <- match.arg(subscales, c("Age", "Gender"), several.ok = TRUE)
  }
  dots <- list(...)
  show_month <- TRUE
  if("show_month" %in% names(dots)){
    show_month <- dots$show_month
  }

  elts <- c()
  if ("Gender" %in% subscales) {
    elts <-
      psychTestR::join(
        elts,
        psychTestR::new_timeline(
          psychTestR::join(
            psychTestR::NAFC_page(
              label = "q4",
              prompt = psychTestR::i18n("TDEG_0004_PROMPT"),
              choices = sprintf("btn%d_text", 1:4),
              labels = purrr::map(
                sprintf("TDEG_0004_CHOICE%d", 1:4),
                psychTestR::i18n
              ),
              button_style = "min-width: 188px"
            )
          ),
          dict = dict,
          default_lang = "ja"
        )
      )
  }
  if ("Age" %in% subscales) {
    elts <-
      psychTestR::join(
        elts,
        psychTestR::new_timeline(
          psychTestR::join(
            ja_year_and_month_select_page(
              label = "q9",
              prompt = psychTestR::i18n("TDEG_0010_PROMPT"),
              min_year = year_range[1],
              max_year = year_range[2],
              show_month = show_month,
            )
          ),
          dict = dict,
          default_lang = "ja"
        )
      )
  }

  psychTestR::join(
    psychTestR::begin_module(label = label),
    elts,
    psyquest:::scoring(
      questionnaire_id = questionnaire_id,
      label = label,
      items = psyquest::get_items(questionnaire_id, subscales),
      subscales = subscales
    ),
    psychTestR::end_module()
  )
}

#' New Japanese year and month select page
#'
#' Create a Japanese year and month select page.
#'
#' @inheritParams psyquest::month_and_year_select_page
#'
ja_year_and_month_select_page <-
  function(
    label,
    prompt,
    save_answer = TRUE,
    min_year = 1930,
    max_year = 2013,
    show_month = TRUE,
    validate = psyquest:::month_and_year_select_page.validate(show_month),
    hide_response_ui = FALSE,
    response_ui_id = "response_ui",
    on_complete = NULL,
    admin_ui = NULL,
    failed_validation_message = psychTestR::i18n(
      ifelse(show_month, "SELECT_MONTH_AND_YEAR", "SELECT_YEAR")
    )
  ) {
    stopifnot(
      is.scalar.character(label)
    )

    ui <-
      shiny::div(
        tagify(prompt),
        make_ui_ja_year_and_month_select(
          id = response_ui_id,
          min_year = min_year,
          max_year = max_year,
          show_month = show_month
        )
      )

    get_answer <- function(input, ...) {
      c(input$month, input$year)
    }

    psychTestR::page(
      ui = ui,
      label = label,
      get_answer = get_answer,
      save_answer = save_answer,
      validate = validate,
      on_complete = on_complete,
      final = FALSE,
      admin_ui = admin_ui
    )
  }

#' Make year and month selectboxes adapted to Japanese
#'
#' Creates html code for year and month selectboxes.
#'
#' @param id HTML ID for the div containing the selectboxes.
#'
#' @param min_year minimum year to display in the year selectbox.
#'
#' @param max_year maximum year to display in the year selectbox.
#'
#' @param show_month (flag) whether or not to display the selectbox for month.
make_ui_ja_year_and_month_select <- function(id = "response_ui",
                                             min_year = 1930,
                                             max_year = 2013,
                                             show_month = TRUE) {
  stopifnot(
    is.scalar.character(id),
    max_year >= min_year,
    is.scalar.logical(show_month)
  )

  months <- c("SELECT_MONTH", "JANUARY", "FEBRUARY", "MARCH", "APRIL", "MAY", "JUNE", "JULY", "AUGUST", "SEPTEMBER", "OCTOBER", "NOVEMBER", "DECEMBER")
  month_numbers <- c(NA, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  months <- setNames(month_numbers, purrr::map(months, psychTestR::i18n))
  years <-
    c(psychTestR::i18n("SELECT_YEAR"),
      paste0(rev(c(min_year:max_year)), "年"))
  years_numbers <- c(NA, rev(c(min_year:max_year)))
  years <- setNames(years_numbers, years)

  outer_div <-
    shiny::tags$div(id = id)

  selectboxes <-
    shiny::tags$div(
      outer_div,
      shiny::selectizeInput(
        "year",
        label = psychTestR::i18n("YEAR"),
        choices = years,
        multiple = FALSE
      ),
      if (show_month) {
        shiny::selectizeInput(
          "month",
          label = psychTestR::i18n("MONTH"),
          choices = months,
          multiple = FALSE
        )
      }
    )

  shiny::tags$div(
    id = "rb",
    style = "width: 300px",
    selectboxes,
    psychTestR::trigger_button("next", psychTestR::i18n("CONTINUE"))
  )
}
