library(deSolve)

## model: rate of change v1 = 0, v2 consumed at first-order rate
semibatch <- function(t, y, p) {
  with(as.list(c(y, p)), {
    growth <- r * (1 - x/K)
    dx  <- growth * x - d * x  # population in the experimental vessel
    dx2 <- growth * x2         # theoretical total population development
    dV <- 0                    # reference Volume
    list(c(dx, dx2, dV), r = growth)
  }) 
}

eventfun <- function(t, y, p){
  with(as.list(c(y, p)), {
    return(c(x / D, x2 / D, V * D)) # D = 2 --> x / 2 and V * 2
  })
}

events <- seq(3, 100, 3.5)

yini   <- c(x = 1, x2 = 1, V = 1)     
parms  <- c(r = 1.6, d = .2, K = 10, D = 2)
times  <- seq(0, 103, by = 0.1)


#out <- ode(func = semibatch, y = yini, times = times, parms = parms)

times <- cleanEventTimes(times, events)
times <-sort(c(times, events))

i <-0
for(r in seq(0.0, 2, 0.02)) {
i <- i+1


parms["r"] <- r

system.time(
out <- ode(func = semibatch, y = yini, times = times, parms = parms, 
  events = list(func = eventfun, time = events), method = "adams")
)

png(file=paste("img", i+1000, ".png", sep=""))  
plot(out, log = "y", 
  ylim=list(c(0.01, 12), c(1, 1e10), c(1, 1e9), c(0.1, 2)),
  main=list("Pop.", "cum. Pop.", "ref. Vol.", "r"))
dev.off()

oo <- as.data.frame(subset(out[,1:5], out[, 1] > 20))

oo$q <- oo$x2 * oo$V

m <- lm(log(q) ~ time, data = oo)
#with(oo, plot(t, log(q), pch="."))
#abline(m)

summary(m)
print(coef(m))

## doubling time
cat("doubling times", log(2)/parms["r"], "->", log(2)/coef(m)[2], "\n")
cat("generations / year", 365/(log(2)/parms["r"]), "->", 365/(log(2)/coef(m)[2]), "\n")

r_mean <- mean(oo$r[oo$time > 20 | oo$time < 100])
cat("mean gen.", 365 / (log(2)/r_mean), "\n")

}

#with(oo, plot(t, q, pch="."))
## start with t=20
#with(oo, lines(t,  q[1] * exp((t-20) * coef(m)[2])))

