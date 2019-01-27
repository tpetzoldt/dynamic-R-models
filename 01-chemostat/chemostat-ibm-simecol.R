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
      N    <- nrow(inds)
      S    <- obj$S
      with(parms, {
        r <- rmax * S / (ks + S)

        rnd <- runif(N)
        newinds <- subset(inds, rnd < r * DELTAT)

        ## version 1: divide existing individuals
        dN <- nrow(newinds)
        dS <- - 1/Y * dN

        ## version 2: de-novo creation of individuals
        dN_ <- r * N * DELTAT
        #newinds <- newbact(round(dN_)) # !! round?

        ## for debugging only
        #cat(N, r, dN, dN_, "\n")

        ## safeguard, don't divide if resource would become negative
        if (S < dS) {
          dS <- 0
          newinds <- NULL
        }

        inds <- rbind(inds, newinds)
        S <- S + dS
        list(inds=inds, S=S)
      })
    }
  ),
  parms = list(
    rmax = 0.5,
    ks = 500,
    D = 0.2,
    S0 = 1000,
    Y = 1
  ),
  init = list(inds = data.frame(age=rep(0, 1000)), S = 1000),
  times = c(from=0, to=100, by=.1),
  solver = "iteration"
)

observer(ibm_test) <- function(state, time, i, out, y) {
  S <- state$S
  N <- nrow(state$inds)
  if ((i %% 20) == 0) cat(i, "S=", S, "N=", N, "\n")

  if (N > 1e5) stop("N > 1e6\n")
  if (S <0 ) stop ("S < 0")

  c(time=time, N=N, S=S)
}

ibm_test <- sim(ibm_test)

o <- out(ibm_test)
matplot(o$time, o[c("N", "S")], xlab="time", ylab="N, S", type="l")
head(o)
