library(psychTestR)

elts <-
  psychTestR::join(
    # one_button_page("Hello"),
    psychTestR::reactive_page(
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
        psychTestR::one_button_page(
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
            psychTestR::set_global(key = "uses", value = url_params$uses, state = state)
            psychTestR::set_global(key = "udes", value = url_params$udes, state = state)
          }
        )
      }
    ),
    psychTestR::reactive_page(
      function(state, ...) {
        psychTestR::one_button_page(
          shiny::p(
            sprintf(
              "https://soscisurvey.de/vocaloidproject/?uses=%s&udes=%s",
              psychTestR::get_global(key = "uses", state = state),
              psychTestR::get_global(key = "udes", state = state)
            )
          )
        )
      }
    ),
    psychTestR::final_page("Finish!")
  )

psychTestR::make_test(
  elts = elts,
  opt = psychTestR::test_options(title = "URL params", admin_password = "1234",languages = c("de_f", "de", "en", "ja"))
)
