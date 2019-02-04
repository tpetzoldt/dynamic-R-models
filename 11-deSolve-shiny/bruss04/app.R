## @knitr brusselator_4
library("deSolve")
library("cOde") # 3rd party package by Daniel Kaschek

## helper function, adapted from package cOde v.2.2 (GPL >= 2.0)
loadDLL <- function(func, cfunction="derivs") {
  .so <- .Platform$dynlib.ext
  checkDLL <- try(getNativeSymbolInfo(cfunction), silent=TRUE)
  if(inherits(checkDLL, "try-error")) {
    dyn.load(paste0(func, .so))
    #cat("Shared object is loaded and ready to use\n")
  } else if ((is.null(checkDLL$package))) {
    # We are on Windows (always overload)
    dyn.load(paste0(func, .so))
  } else if((checkDLL$package)[[1]] != func) {
    # We are on Unix
    dyn.load(paste0(func, .so))
  }
}

compile <- !file.exists(paste0("brusselator", .Platform$dynlib.ext))

brussC <- funC(c(
  X = "k1*A - k2*B*X + k3*X^2*Y - k4*X",
  Y = "k2*B*X - k3*X^2*Y"
), modelname="brusselator", compile=compile)

if (!compile) loadDLL("brusselator")

y0 <- c(X=1, Y=1)
parms <- c(A=1, B=3, k1=1, k2=1, k3=1, k4=1)

makelist <- function(i, obj, min=NA, max=NA, step=NA, width=NULL) {
  list(inputId=names(obj[i]), label=names(obj[i]),
       value=unname(obj[i]), min=min, max=max, step=step,
       width=width)
}

L_y0    <- lapply(1:length(y0), makelist, obj=y0, min=0, max=10, step=0.2, width=100)
L_parms <- lapply(1:length(parms), makelist, obj=parms, min=0, max=10, step=0.2, width=100)

server <- function(input, output) {
  output$brussels <- renderPlot({
    L_input <- reactiveValuesToList(input)
    y0    <- with(L_input, c(X=X, Y=Y))
    parms <- with(L_input, c(A=A, B=B, k1=k1, k2=k2, k3=k3, k4=k4))
    times <- seq(0, 100, .1)
    #out <- ode(y0, times, brusselator, parms) # R version
    out <- odeC(y0, times, brussC, parms)      # C version
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
      lapply(L_y0, function(x) do.call("numericInput", x)),
      h3("Parameters"),
      lapply(L_parms, function(x) do.call("numericInput", x))
    ),
    mainPanel(
      h3("Simulation results"),
      plotOutput("brussels")
    )
  )
)

shinyApp(ui = ui, server = server)
