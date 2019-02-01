library("deSolve")
library("rootSolve")

chemostat <- function(time, init, parms) {
  with(as.list(c(init, parms)), {
    mu   <- mumax * P/(kp + P)  # Monod equation
    dAlg <- mu * Alg - D * Alg
    dP   <-  D *(P0 - P) - 1/Y * mu * Alg
    list(c(dAlg, dP), mu=mu)
   })
}
parms <- c(
  mumax = 0.5,    # 1/d
  kp    = 0.01,   # half saturation constant (mg/L)
  Y     = 41,     # yield coefficient (stoichiometric C:P ratio)
  D     = 0.1,    # 1/d
  P0    = 0.05    # P in inflow (mg/L)
)
times <- seq(0, 40, 0.1)  # (d)
init  <- c(Alg=0.01, P=0.05) # Phytoplankton C and Phosphorus P (mg/L)

## =============================================================================
## Dynamic simulation
## =============================================================================
out <- ode(init, times, chemostat, parms)
plot(out)

## =============================================================================
## Steady state solution
## =============================================================================
state <- data.frame(
  D = seq(0, 0.6, length.out = 100),
  X = 0,
  S = 0
)

for (i in 1:nrow(state)) {
  parms["D"] <- state$D[i]
  times <- c(0, Inf)
  out <- runsteady(init, times, chemostat, parms)
  state[i, 2:3] <- out$y
}

par(mfrow = c(3, 1))
plot(S ~ D, data = state, type = "l")
plot(X ~ D, data = state, type = "l")
plot(S * X ~ D, data = state, type = "l")
