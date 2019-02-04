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

