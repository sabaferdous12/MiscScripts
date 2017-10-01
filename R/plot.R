initial.dir<-getwd()
setwd("/export/people/Saba/Simulation_Analysis/R")
library(nlme)

wt<-read.table("4M48WtPep_1000_ss.xpm_ss.txt", header=F, sep=":")
x<-wt$V1
y<-wt$V2
plot(y)
lines(y)
axis(1, at = x, labels=T)
sink("image.out")

sink()
detach("package:nlme")
setwd(initial.dir)
