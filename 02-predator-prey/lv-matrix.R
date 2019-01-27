library(deSolve)

model <- function(t, n, parms) {
  with(parms, {
    dn <- r * n  + n * (A %*% n)
    list(dn)
  })
}

parms <- list(
  r = c(r1 = 0.1, r2 = 0.1, r3 = -0.1, r4 = -0.1),
  A = matrix(c(
    0.0, 0.0, -0.2, 0.0, # prey 1
    0.0, 0.0, 0.0, -0.1, # prey 2
    0.2, 0.0, 0.0, 0.0,  # predator 1; eats prey 1
    0.0, 0.1, 0.0, 0.0), # predator 2; eats prey 2
    nrow = 4, ncol = 4, byrow = TRUE)
)

times = seq(0, 500, 0.1)
n0  = c(n1 = 1, n2 = 1, n3 = 2, n4 = 2)

out <- ode(n0, times, model, parms)
plot(out)
### ----------------------------------------------------------------------------

## Now change the interactions, so that preys eat on both resources

## weak coupling
A <- matrix(c(0.0,  0.0, -0.1,   -0.001,      # prey 1
              0.0,  0.0, -0.002, -0.2,        # prey 2
              0.0,  0.0,  0.0,    0.0,        # predator 1
              0.0,  0.0,  0.0,    0.0),       # predator 2
              nrow = 4, ncol = 4, byrow=TRUE)

parms$A <- A - t(A) # make matrix symmetric
out1 <- ode(n0, times, model, parms)

## stronger coupling
A <- matrix(c(0.0, 0.0,  -0.1,  -0.05,       # prey 1
              0.0, 0.0,  -0.02, -0.2,        # prey 2
              0.0, 0.0,   0.0,   0.0,        # predator 1
              0.0, 0.0,   0.0,   0.0),       # predator 2
              nrow = 4, ncol = 4, byrow=TRUE)

parms$A <- A - t(A) # make matrix symmetric

out2 <- ode(n0, times, model, parms)
plot(out, out1, out2)

## Exercises:
## - experiment with strength of interaction
## - let predators prey on themselves
## - expand the model to more species
