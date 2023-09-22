## Demo of a logistic model with horizontal flux

library("deSolve")
library("ReacTran")
library("gifski")


## The model equations
logistic2d <- function (t, state, parms)  {
  z <- matrix(state, nrow = N)

  ## use package reactran for the transport or differences of shifted matrices
  if (reactran) {
    dflux_z <- tran.2D(z, dx = dx, dy = dy, D.x = D, D.y = D)$dC
  } else {
    flux_zx <- -D * rbind(zero, (z[2:N,] - z[1:(N-1),]), zero)/dx
    flux_zy <- -D * cbind(zero, (z[,2:N] - z[,1:(N-1)]), zero)/dy

    dflux_z  <- (flux_zx[1:N,] - flux_zx[2:(N+1),])/dx +
                (flux_zy[,1:N] - flux_zy[,2:(N+1)])/dy


  }
  ## state equation
  dz <- r * z * (1 - z/K) + dflux_z
  list(dz)
}

## grid
N    <- 101        # N^2 is the number of grid cells
zero <- rep(0, N)
dx   <- 10/N
dy   <- 10/N
ndx  <- 1:N^2      # index for accessing an individual state variable

## model parameters
K <- 100
r <- 0.1
D <- 1e-3       # Diffusivity

# initial conditions
z0 <- 1
z  <- matrix(nrow = N, ncol = N, data = 0)

z[51, 51] <- z0


times <- 0:200

# use ReacTran for the transport?
reactran <- TRUE

system.time(
  out <- ode.2D (y = z, func = logistic2d, t = times, nspec=1, parms = NULL,
                dim = c(N, N), method = "adams")
)

## first column is time
ndx <- 2:(N^2 + 1)

## write images and create an animated gif
for (i in times) {
  png(paste0("z", i+1000, ".png"))
  image(matrix(out[i + 1, ndx], nrow=N), main=i, col=topo.colors(20))
  dev.off()
}

gifski(dir(pattern="^z.*png$"), gif_file = "animation.gif", width=800, height=800, delay=1/25)
