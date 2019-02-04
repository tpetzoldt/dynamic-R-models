## workaround for shinyapps.io
#if(!require(deSolve)) {
#  install.packages("deSolve", repos="https://cloud.r-project.org/")
#}

## @knitr brusselator_3
library("deSolve")

brusselator <- function(t, y, p) {
  with(as.list(c(y, p)), {
    dX <- k1*A - k2*B*X + k3*X^2*Y - k4*X
    dY <- k2*B*X - k3*X^2*Y
    list(c(X=dX, Y=dY))
  })
}

## prepare data structures to create UI programmatically
y0    <- c(X=1, Y=1)
parms <- c(A=1, B=3, k1=1, k2=1, k3=1, k4=1)

makelist <- function(i, obj, min=NA, max=NA, step=NA, width=NULL) {
  list(inputId=names(obj[i]), label=names(obj[i]),
       value=unname(obj[i]), min=min, max=max, step=step,
       width=width)
}

## two lists of lists
L_parms <- lapply(1:length(parms), makelist, obj=parms, min=0, max=10, step=0.2, width=100)
L_y0 <- lapply(1:length(y0), makelist, obj=y0, min=0, max=10, step=0.2, width=100)

server <- function(input, output) {
  output$brussels <- renderPlot({
    L_input <- reactiveValuesToList(input) # to enable width
    y0    <- with(L_input, c(X=X, Y=Y))
    parms <- with(L_input, c(A=A, B=B, k1=k1, k2=k2, k3=k3, k4=k4))
    times <- seq(0, 100, .1)
    out <- ode(y0, times, brusselator, parms)
    par(mfrow=c(1, 2))
    matplot.0D(out, main="time series")
    plot(out[,2:3], type="l", xlab="A", ylab="B", main="state diagram")
  })
}

ui <- fluidPage(
  headerPanel("Brusselator: generic UI creation"),
  sidebarLayout(
    sidebarPanel(
      ## generic creation of UI elements
      h3("Init values"),
      lapply(L_y0, function(x) do.call("numericInput", x)),   # <--------

      h3("Parameters"),
      lapply(L_parms, function(x) do.call("numericInput", x)) # <--------
    ),
    mainPanel(
      h3("Simulation results"),
      plotOutput("brussels")
    )
  )
)

shinyApp(ui = ui, server = server)
