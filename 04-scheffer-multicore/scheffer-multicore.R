## Alternative stable states in shallow lakes
##
## Scheffer, M.; Hosper, S.; Meijer, M.; Moss, B. & Jeppesen, E. Alternative
## equilibria in shallow lakes Trends in Ecology & Evolution, Elsevier, 1993, 8,
## 275-279
##
## Scheffer, M. Alternative Stable States in Eutrophic Freshwater Systems: A
## Minimal Model. Hydrobiol. Bull., 1989, 23, 73-83


library(rootSolve)
library(parallel)    # requires a recent version of R

equilibrium <- function(N, A0) {
  require(rootSolve)
  vegturbde <- function(t, A, parms, N){
    with(parms, {
      V  <- hA^p / (hA^p + A^p)
      dA <- r * A * N / (N + hN) * hV / (hV + V) - c * A^2
      list(dA)
    })
  }
  parms     <- list(hA = 0.5, p = 10, r = 1, hN = 1, hV = 0.5, c = 0.5)
  runsteady(y = A0, func = vegturbde, parms = parms, N = N)$y
}

## increase length for a CPU stresstest
nutrients <- seq(0.001, 2, length = 200)

## without parallelisation
#system.time({
#  eutrophication <- sapply(nutrients, equilibrium, A0 = 0.1)
#  restoration    <- sapply(nutrients, equilibrium, A0 = 1.5)
#})

## parallelised version; set number of cores to 1, 2, 4, 8, ...
cl <- makeCluster(getOption("cl.cores", 4))

system.time({
  eutrophication <- unlist(parLapply(cl, nutrients, equilibrium, A0 = 0.1))
  restoration    <- unlist(parLapply(cl, nutrients, equilibrium, A0 = 1.5))
})
stopCluster(cl)

## alternative parallelised version, even simpler but Linux only
#  eutrophication <- unlist(mclapply(nutrients, equilibrium, A0 = 0.1, mc.cores = 8))
#  restoration    <- unlist(mclapply(nutrients, equilibrium, A0 = 1.5, mc.cores = 8))


#pdf("hysteresis.pdf", width=8, height=8)
par(cex=2.2, lwd=3, mar=c(4,4,0,0)+.1)
matplot(nutrients, cbind(eutrophication, restoration), type = "l", ylab = "algae", lwd=3, las=1)
legend("topleft", c("eutrophication", "restoration"), col=1:2, lty=c(1,2), bty="n")
#dev.off()
