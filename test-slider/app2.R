ui <- fluidPage(
  shiny::tags$head(
    tags$style(HTML(".js-irs-0 .irs-single , .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: transparent; border: transparent;}"))
  ),
  sliderInput(inputId="test", label=NULL, min=1, max=10, value=5, step = 1, width='100%')
)

server <- function(input, output, session){
  #
}

shinyApp(ui = ui, server=server)
