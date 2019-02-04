## @knitr brusselator_4_core
library("deSolve")
library("cOde")    # 3rd party package by Daniel Kaschek

y0 <- c(X=1.1, Y=1)
times <- seq(0, 100, .1)
parms <- c(A=1, B=3, k1=1, k2=1, k3=1, k4=1)

brussC <- funC(c(
    X = "k1*A - k2*B*X + k3*X^2*Y - k4*X",
    Y = "k2*B*X - k3*X^2*Y"
), modelname="brusselator", compile=TRUE)

out <- odeC(y0, times, brussC, parms)
matplot(out[,1], out[,2:3], type="l")

