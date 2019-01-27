library(simecol)

ibm_test <- new("indbasedModel",
  main = function(time, obj, parms) {
    #if (parms$D > max(parms$r, parms$DELTAT)) stop("time step to large") # CFL criterion
    obj <- live(obj, parms)
    obj <- dilute(obj, parms)
    obj <- divide(obj, parms)
    obj
  },
  equations = list(
    newbact = function(n) {
      if (n > 0) {
        data.frame(age = rep(0, n))
      } else {
        NULL
      }
    },
    live = function(obj, parms){
      obj$inds$age    <- obj$inds$age + parms$DELTAT
      obj
    },
    dilute  = function(obj, parms) {
      S    <- obj$S
      inds <- obj$inds
      N    <- nrow(inds)

      with(parms, {
        rnd <- runif(N)
        inds <- subset(inds, D * DELTAT < rnd)
        S <- S + D * (S0 - S) * DELTAT
        list(inds=inds, S=S)
      })
    },
    divide = function(obj, parms) {
      inds <- obj$inds
      N <- nrow(inds)
      S <- obj$S
      with(parms, {
        r <- rmax * S / (ks + S)
        dN <- r * N  * DELTAT
        dS <- - 1/Y * dN

        ## safeguard, don't divide if resource would become negative
        if (S < dS) {
          dS <- 0
          dN <- 0
        }
        newinds <- newbact(round(dN)) # !! round?

        inds <- rbind(inds, newinds)
        S <- S + dS
        list(inds=inds, S=S)
      })
    }
  ),
  parms = list(
    rmax = 0.5,
    ks = 100,
    D = 0.1,
    S0 = 1000,
    Y = 1
  ),
  init = list(inds = data.frame(age=rep(0, 1000)), S = 1000),
  times = c(from=0, to=100, by=.2),
  solver = "iteration"
)

observer(ibm_test) <- function(state, time, i, out, y) {
  S <- state$S
  N <- nrow(state$inds)
  c(time=time, N=N, S=S)
}

ibm_test <- sim(ibm_test)

o <- out(ibm_test)
matplot(o$time, o[c("N", "S")], xlab="time", ylab="N, S, age", type="l")
