## @knitr brusselator_1
library("deSolve")

brusselator <- function(t, y, p) {
  with(as.list(c(y, p)), {
    dX <- k1*A   - k2*B*X    + k3*X^2*Y - k4*X
    dY <- k2*B*X - k3*X^2*Y
    list(c(X=dX, Y=dY))
  })
}

server <- function(input, output) {
  output$brussels <- renderPlot({
    parms <- c(A=input$A, B=input$B, k1=1, k2=1, k3=1, k4=1)
    out <- ode(y = c(X=1, Y=1), times=seq(0, 100, .1), brusselator, parms)
    matplot.0D(out)
  })
}

ui <- fluidPage(
  numericInput("A", label = "A", value = 1),
  numericInput("B", label = "B", value = 3),
  plotOutput("brussels")
)

shinyApp(ui=ui, server=server)
