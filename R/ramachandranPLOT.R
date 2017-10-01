library(MASS)

args <- (commandArgs(TRUE))
print(args[1])

proline <- read.table(paste(args[1]),sep="",header=F,quote="")
names(proline) <- c("Phi","Psi","Pro")
proline.density <- kde2d(proline$Phi,proline$Psi, n=361, lims=c(-180,180,-180,180), h=c(10,10))
summary(proline.density)

sum(proline.density$z)
max(proline.density$z)
min(proline.density$z)

proline.total <- sum(proline.density$z)
100*sum(proline.density$z[proline.density$z > 0.00000123]) / proline.total
100*sum(proline.density$z[proline.density$z > 0.00000726]) / proline.total

png('ramachandraPRO.png')
filled.contour(proline.density, xlab=expression(phi), ylab=expression(psi),
  main="Proline", levels=c(0,0.00000123,0.00000726,1),
  xlim=c(-180,180), ylim=c(-180,180), asp=1,
  col=c('#FFFFFF','#C0FFC0','#80FF80'))
dev.off()

