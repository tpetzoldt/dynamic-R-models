### ============================================================================
###  Individual-based implementation of a chemostat -- plain R
###    S:    substrate
###    inds: table of individuals (data frame)
###    rmax: maximum growth rate
###  ks: half saturation constant of Monod function
###    D:    dilution rate
### ============================================================================


main <- function(time, obj, parms) {
  obj <- live(obj, parms)
  obj <- dilute(obj, parms)
  obj <- divide(obj, parms)
  obj
}


newbact <- function(n) {
  if (n > 0) {
    data.frame(age = rep(0, n))
  } else {
    NULL
  }
}

live <- function(obj, parms){
  obj$inds$age    <- obj$inds$age + parms$DELTAT
  obj
}

dilute  <- function(obj, parms) {
  S    <- obj$S
  inds <- obj$inds
  N    <- nrow(inds)

  with(parms, {
    rnd <- runif(N)
    inds <- subset(inds, D * DELTAT < rnd)
    S <- S + D * (S0 - S) * DELTAT
    list(inds=inds, S=S)
  })
}

divide <- function(obj, parms) {
  inds <- obj$inds
  N    <- nrow(inds)
  S    <- obj$S
  with(parms, {
    r <- rmax * S / (ks + S)

    ## cell division
    rnd <- runif(N)
    ndx <- (rnd < r * DELTAT)
    newinds <- subset(inds, ndx)

    # set age of new individuals to zero
    if (length(newinds) > 0) newinds$age <- 0

    ## resource consumption
    dN <- nrow(newinds)
    dS <- - 1/Y * dN

    ## safeguard: divide only if resource remains positive
    if (S >= dS) {
      inds$age[ndx] <- 0            # set age of divided cells to zero
      S <- S + dS                   # update resource
      inds <- rbind(inds, newinds)  # add new individuals to population
    }

    list(inds=inds, S=S)
  })
}


parms = list(
  rmax = 0.5,
  ks = 500,
  D = .1,
  S0 = 1000,
  Y = 1
)

state <- list(inds = data.frame(age=rep(0, 1000)), S = 1000)

times <- seq(0, 100, .1)

o <- data.frame(time=times[1], N=nrow(state$inds), S=state$S, maxage=max(state$inds$age))

for (i in 2:length(times)) {
  parms$DELTAT <- times[i] - times[i-1]
  state <- main(times[i], state, parms)
  ret <- c(time = times[i], N=nrow(state$inds), S=state$S, maxage=max(state$inds$age))
  o <- rbind(o, ret)
}


matplot(o$time, o[c("N", "S", "maxage")], xlab="time", ylab="N, S", type="l")
head(o)

