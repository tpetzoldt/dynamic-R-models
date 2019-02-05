
## run all the scenarios on 4 CPU cores in parallel
library(parallel)
D <- rep(seq(0, 0.4, 0.01), each=10)

cl <- makeCluster(getOption("cl.cores", 4))

ret <- parLapply(cl, 1:length(D), function(i) {
  source("chemostat-ibm-simecol.R")

  observer(ibm_test) <- function(state, time, i, out, y) {
    S <- state$S
    N <- nrow(state$inds)
    c(time=time, N=N, S=S)
  }

  D <- rep(seq(0, 0.4, 0.01), each=10)
  parms(ibm_test)["D"] <- D[i]
  cbind(run=i, D=D[i], out(sim(ibm_test)))
})
stopCluster(cl)

longtab <- rbind.fill(ret)

steady <- longtab[longtab$time == max(longtab$time), ]

ggplot(steady, aes(D, N)) + geom_point()
ggplot(steady, aes(D, S)) + geom_point()
ggplot(steady, aes(D, D*N)) + geom_point()


