library(deSolve)
library(scatterplot3d)

## Chaos in the atmosphere
Lorenz <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dX <-  a * X + Y * Z
    dY <-  b * (Y - Z)
    dZ <- -X * Y + c * Y - Z
    list(c(dX, dY, dZ))
  })
}

parameters <- c(a = -8/3, b = -10, c = 28)
state      <- c(X = 1, Y = 1, Z = 1)
times      <- seq(0, 100, by = 0.01)

out <- ode(y = state, times = times, func = Lorenz, parms = parameters)

scatterplot3d(out[,-1], type = "l", color="#D2DC46", zlim=c(-50, 30), xlim=c(0,60))

pdf("butterfly.pdf", width=5, height=5)
#scatterplot3d(out[,-1], type = "l", axis=FALSE, color="#D2DC46", zlim=c(-90, 30), xlim=c(0,60))
scatterplot3d(out[,-1], type = "l", axis=FALSE, color="#D2DC46", zlim=c(-90, 30), xlim=c(0,60))
dev.off()

