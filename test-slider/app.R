library("shiny")
library("shinyWidgets")

ui <- fluidPage(
  br(),
  h4("Wie klingt diese Stimme?"),
  br(),
  sliderInput(
    inputId = "mySliderText",
    label = div(style='width:300px;',
                div(style='float:left;', 'künstlich'),
                div(style='float:right;', 'menschlich')),
    min = 1,
    max = 100,
    value = 50,
    step = 1,
    ticks = F,
    width = '300px'
  )
)

server <- function(input, output, session) {
  output$result <- renderPrint(str(input$mySliderText))
}

shinyApp(ui = ui, server = server)
