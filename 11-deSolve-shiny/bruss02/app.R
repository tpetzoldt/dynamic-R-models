## @knitr brusselator_2
library("deSolve")

brusselator <- function(t, y, p) {
  with(as.list(c(y, p)), {
    dX <- k1*A - k2*B*X + k3*X^2*Y - k4*X
    dY <- k2*B*X - k3*X^2*Y
    list(c(X=dX, Y=dY))
  })
}


server <- function(input, output) {
  output$brussels <- renderPlot({
    y0 <- c(X=input$X, Y=input$Y)
    parms <- c(A=input$A, B=input$B,
               k1=input$k1, k2=input$k2, k3=input$k3, k4=input$k4)
    times <- seq(0, 100, .1)
    out <- ode(y0, times, brusselator, parms)
    par(mfrow=c(1, 2))
    matplot.0D(out, main="time series")
    plot(out[,2:3], type="l", xlab="A", ylab="B", main="state diagram")
  })
}

ui <- fluidPage(
  headerPanel("Brusselator"),
  sidebarLayout(
    sidebarPanel(
      h3("Init values"),
      numericInput("X", label = "X",
                   min = 0.0, max = 5,  value = 1, step = 0.2, width=100),
      numericInput("Y", label = "Y",
                   min = 0.0, max = 5,  value = 1, step = 0.2, width=100),

      h3("Parameters"),
      numericInput("A", label = "A",
                   min = 0.0, max = 5,  value = 1, step = 0.2, width=100),
      numericInput("B", label = "B",
                   min = 0.0, max = 5,  value = 3, step = 0.2, width=100),
      numericInput("k1", label = "k1",
                   min = 0.0, max = 10, value = 1, step = 0.2, width=100),
      numericInput("k2", label = "k2",
                   min = 0.0, max = 10, value = 1, step = 0.2, width=100),
      numericInput("k3", label = "k3",
                   min = 0.0, max = 10, value = 1, step = 0.2, width=100),
      numericInput("k4", label = "k4",
                   min = 0.0, max = 10, value = 1, step = 0.2, width=100)
    ),
    mainPanel(
      h3("Simulation results"),
      plotOutput("brussels")
    )
  )
)

shinyApp(ui = ui, server = server)
