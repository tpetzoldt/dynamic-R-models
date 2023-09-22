library(deSolve)

SIR <- function(t, x, parms) {
  with(as.list(c(parms, x)), {
    dS <- - beta * I * S
    dI <- beta * I * S - nu * I
    dR <- nu * I
    list(c(dS, dI, dR))
  })
}

## Parameters
parms  <- c(beta = 0.001, nu = 0.02)
x <- c(S=1000, I=1, R=0)

## vector of timesteps
times  <- seq(0, 100, length = 101)

out <- ode(x, times, SIR, parms)
plot(out)
