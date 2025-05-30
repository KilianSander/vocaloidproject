% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test-elements.R
\name{vas_page}
\alias{vas_page}
\title{New Visual Analog Scale Page}
\usage{
vas_page(
  label,
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
  post = NULL
)
}
\arguments{
\item{label}{Label for the current page (character scalar).}

\item{prompt}{Prompt to display (character scalar or Shiny tag object).}

\item{min_label}{(scalar character) Label for the left endpoint of the scale,
which corresponds to the minimum value of the slider.}

\item{max_label}{(scalar character) Label for the right endpoint of the
scale, which corresponds to the maximum value of the slider.}

\item{min_numeric}{(scalar numeric) Minimum value for the slider.}

\item{max_numeric}{(scalar numeric) Maximum value for the slider.}

\item{value}{(scalar numeric) Initial value of the slider.
Must lie between \code{min} and \code{max}.}

\item{save_answer}{Whether or not to save the answer.}

\item{button_text}{Text for the submit button (character scalar).}

\item{on_complete}{Optional function to execute on leaving the page
(after successful validation).
The argument list should include \code{...},
and any of:
\code{state}, the participant's state object;
\code{answer}, the participant's most recent answer;
\code{input}, the current page's Shiny input object;
\code{session}, the current Shiny session object;
\code{opt}, the test's option list as created by \code{test_options()}.}

\item{admin_ui}{Optional UI component for the admin panel.}

\item{step}{Specifies the interval between each selectable value on the
slider. Either \code{NULL}, the default, which uses a heuristic to determine the
step size or a single number. If the values are dates, \code{step} is in days;
if the values are date-times, \code{step} is in seconds.}

\item{hide_numeric_values}{(logical scalar) Whether or not to hide the
numeric values in the UI, that is, \code{min_numeric}, \code{max_numeric}, and \code{value}.}

\item{fill_bar}{(logical scalar) \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
Whether or not to fill the bar from
the minimum to the position of the slider.
Currently only works for the first occurrence of a slider.}

\item{reverse}{(logical scalar) Whether or not the slider should be reversed.
Usually (i.e., \code{reversed = FALSE}) \verb{min_*} is left and \verb{max_*} is right.
When reversed (i.e., \code{reversed = TRUE}),
\verb{min_*} is right and \verb{max_*} is left.}

\item{round}{\code{TRUE} to round all values to the nearest integer;
\code{FALSE} if no rounding is desired; or an integer to round to that
number of digits (for example, 1 will round to the nearest 10, and -2 will
round to the nearest .01). Any rounding will be applied after snapping to
the nearest step.}

\item{ticks}{\code{FALSE} to hide tick marks, \code{TRUE} to show them
according to some simple heuristics.}

\item{animate}{\code{TRUE} to show simple animation controls with default
settings; \code{FALSE} not to; or a custom settings list, such as those
created using \code{\link[shiny:animationOptions]{animationOptions()}}.}

\item{slider_width}{(scalar) Width of the slider. Must be valid for use as a
CSS unit of length.}

\item{sep}{Separator between thousands places in numbers.}

\item{pre}{A prefix string to put in front of the value.}

\item{post}{A suffix string to put after the value.}
}
\description{
Creates a page where the participant responds by using a slider like a
visual analog scale.
}
