server <- function(input, output) {
  output$distPlot <- renderPlot({
    hist(rnorm(input$obs))
  })
}

ui <- fluidPage(
    sliderInput("obs", "Number of observations:", min=10, max=500, value = 100),
    plotOutput("distPlot")
)

shinyApp(ui = ui, server = server)
