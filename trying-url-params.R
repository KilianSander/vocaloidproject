library(psychTestR)

elts <-
  join(
    # one_button_page("Hello"),
    reactive_page(
      function(state, ...) {
        url_params <- psychTestR::get_url_params(state)
        # set_global(
        #   key = "uses",
        #   state = state,
        #   value = url_params$uses
        # )
        # set_global(
        #   key = "udes",
        #   state = state,
        #   value = url_params$udes
        # )
        one_button_page(
          if (is.null(url_params$uses) | is.null(url_params$udes)) {
            shiny::p("At least 1 URL parameter is missing!")
          } else {
            shiny::div(
              shiny::p(
                sprintf("Welcome to Session %s!",
                        # get_global(key = "uses", state)
                        url_params$uses)
              ),
              shiny::p(
                sprintf("You have Design %s.",
                        # get_global(key = "udes", state)
                        url_params$udes)
              )
            )
          },
          on_complete = function(state, ...) {
            set_global(key = "uses", value = url_params$uses, state = state)
            set_global(key = "udes", value = url_params$udes, state = state)
          }
        )
      }
    ),
    reactive_page(
      function(state, ...) {
        one_button_page(
          shiny::p(
            sprintf(
              "https://soscisurvey.de/vocaloidproject/?uses=%s&udes=%s",
              get_global(key = "uses", state = state),
              get_global(key = "udes", state = state)
            )
          )
        )
      }
    ),
    final_page("Finish!")
  )

make_test(
  elts = elts,
  opt = test_options(title = "URL params", admin_password = "1234")
)
