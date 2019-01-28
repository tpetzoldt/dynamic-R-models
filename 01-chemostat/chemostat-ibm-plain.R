### ============================================================================
###  Individual-based implementation of a chemostat -- plain R
###    S:    substrate
###    inds: table of individuals (data frame)
###    rmax: maximum growth rate
###  ks: half saturation constant of Monod function
###    D:    dilution rate
### ============================================================================


live <- function(time, inds, S, parms, DELTAT){
  inds$age    <- inds$age + DELTAT


  with(parms, {

    ## dilute
    N    <- nrow(inds)
    rnd  <- runif(N)
    inds <- subset(inds, D * DELTAT < rnd)
    S <- S + D * (S0 - S) * DELTAT

    ## cell division
    N    <- nrow(inds)
    r <- rmax * S / (ks + S)
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

inds  <- data.frame(age=rep(0, 1000))
S     <- 1000
parms <- list(rmax = 0.5, ks = 500, D = .1, S0 = 1000,Y = 1)
DELTAT <- 0.1
times <- seq(0, 100, DELTAT)

o <- data.frame(time=times[1], N=nrow(inds), S=S, maxage=max(inds$age))

for (i in 2:length(times)) {

  state <- live(times[i], inds, S, parms, DELTAT)
  inds <- state$inds
  S    <- state$S

  ret <- c(time = times[i], N=nrow(inds), S=S, maxage=max(inds$age))
  o <- rbind(o, ret)
}


matplot(o$time, o[c("N", "S", "maxage")], xlab="time", ylab="N, S", type="l")
head(o)

