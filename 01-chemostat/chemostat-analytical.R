## =============================================================================
## Analytical solution for the steady state
## =============================================================================


mumax = 0.5    # 1/d
ks    = 0.01   # half saturation constant (mg/L)
Y     = 41     # yield coefficient
D     = 0.1    # 1/d
S0    = 0.05   # P in inflow (mg/L)

D <- seq(0, 0.6, length.out = 100)

S <- D * ks / (mumax - D)
S <- ifelse(D > (mumax * S0)/(ks + S0), S0, S)

X <- Y * (S0 - S)

par(mfrow=c(3,1))
plot(D, S, type="l", col="red")
plot(D, X, type="l")
plot(D, X*S, type="l")



