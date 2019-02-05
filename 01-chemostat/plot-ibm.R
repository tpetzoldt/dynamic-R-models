library(simecol)
library(plyr)
library(ggplot2)
source("chemostat-ibm-simecol.R")

#ret <- lapply(1:9, FUN=function(x) cbind(run=x, out(sim(ibm_test))))
#dummy <- lapply(ret, function(o) matplot(o$time, o[c("N", "S")], xlab="time", ylab="N, S", type="l"))

ret <- lapply(1:100, FUN=function(x) cbind(run=x, out(sim(ibm_test))))

ret2 <- rbind.fill(ret)


ggplot(ret2, aes(time, N)) + geom_bin2d(binwidth=c(1,10))
ggplot(ret2, aes(time, S)) + geom_bin2d(binwidth=c(1,10))

ggplot(ret2, aes(time, N)) + geom_hex(bins=100)
ggplot(ret2, aes(time, S)) + geom_hex(bins=100)




ibm <- ibm_test # clone the whole object

observer(ibm) <- function(state, time, i, out, y) {
  S <- state$S
  N <- nrow(state$inds)
  c(time=time, N=N, S=S)
}


## series of _D_ilutions with 10 replicates each
D <- rep(seq(0, 0.4, 0.01), each=10)

## run all the scenarios
ret <- lapply(1:length(D),
              FUN = function(i) {
                cat(D[i], "\n")
                parms(ibm)["D"] <- D[i]
                cbind(run=i, D=D[i], out(sim(ibm)))
                }
              )

longtab <- rbind.fill(ret)

steady <- longtab[longtab$time == max(longtab$time), ]

ggplot(steady, aes(D, N)) + geom_point()
ggplot(steady, aes(D, S)) + geom_point()
ggplot(steady, aes(D, D*N)) + geom_point()



