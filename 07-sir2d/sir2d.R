## Demo of a SIR (Susceptible Infected Recovered) model with horizontal flux

library("deSolve")
library("ReacTran")
library("gifski")


## The model equations
SIR2D <- function (t, state, parms)  {
  S  <- matrix(state[ndx], N, N)
  I  <- matrix(state[N^2 + ndx], N, N)
  R  <- matrix(state[2*(N^2) + ndx], N, N)

  infect <- beta * I * S
  recovr <- nu * I

  dS <- -infect
  dI <- infect - recovr
  dR <- recovr

  if (reactran) {
    dI <- dI + tran.2D(I, dx = dx, dy = dy, D.x = D, D.y = D)$dC
  } else {
    flux_Ix <- -D * rbind(zero, (I[2:N,] - I[1:(N-1),]), zero)/dx
    flux_Iy <- -D * cbind(zero, (I[,2:N] - I[,1:(N-1)]), zero)/dy

    dflux_I  <- (flux_Ix[1:N,] - flux_Ix[2:(N+1),])/dx +
                (flux_Iy[,1:N] - flux_Iy[,2:(N+1)])/dy

    dI <- dI + dflux_I
  }
  list(c(dS, dI, dR))
}

## grid
N    <- 101        # N^2 is the number of grid cells
zero <- rep(0, N)
dx   <- 10/N
dy   <- 10/N
ndx  <- 1:N^2      # index for accessing an individual state variable

## model parameters
beta  <- 0.001
nu    <- 0.02
D     <- 1e-3       # Diffusivity

# initial conditions
S0 <- 100; I0 <- 1; R0 <- 0.0

S  <- matrix(nrow = N, ncol = N, data = S0)
I  <- matrix(nrow = N, ncol = N, data = 0)
R  <- matrix(nrow = N, ncol = N, data = R0)

set.seed(123)
I[sample(1:length(I), 10)] <- I0

y <- c(S = S, I = I, R = R)

times <- 0:200

# use ReacTran for the transport?
reactran <- FALSE

system.time(
  out <- ode.2D (y = y, func = SIR2D, t = times, nspec=3, parms = NULL,
                dim = c(N, N), method = "adams")
)

## time column + range of S
ndx <- 1 + (N^2 + 1):( 2 * N^2)

## write images and create an animated gif
for (i in times) {
  png(paste0("infected", i+1000, ".png"))
  image(matrix(out[i + 1, ndx], nrow=N), main=i, col=topo.colors(20))
  dev.off()
}

gifski(dir(pattern="^infected.*png$"), gif_file = "animation.gif", width=800, height=800, delay=1/25)
