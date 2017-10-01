data<-read.csv(file = "test.csv", head=TRUE, sep=",")
dat
data
## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    library(plyr)
    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }
    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
      .fun = function(xx, col) {
        c(N    = length2(xx[[col]], na.rm=na.rm),
          mean = mean   (xx[[col]], na.rm=na.rm),
          sd   = sd     (xx[[col]], na.rm=na.rm)
        )
      },
      measurevar
    )
    # Rename the "mean" column    
    datac <- rename(datac, c("mean" = measurevar))
    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult
    return(datac)
}
tgc <- summarySE(data, measurevar="Avg", groupvars=c("Mutant"))
tgc
 ggplot(tgc, aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0)
library(ggplot2)
 ggplot(tgc, aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0)
 ggplot(tgc, aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=30)
 ggplot(tgc, aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0)
 ggplot(tgc, aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=100)
 ggplot(tgc, aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0)
sorttt<-tgc[order(Mutant)]
sorttt<-tgc[order(Mutant),]
tgc
sorttt<-tgc[order(1), ]
sorttt
sorttt<-tgc[order(2), ]
sorttt
sorttt<-tgc[order(10), ]
sorttt
sorttt<-tgc[order(Avg),]
sorttt<-tgc[order(tag$Avg),]
sorttt<-tgc[order(tgc$Avg),]
sorttt
sorttt<-tgc[order(tag$Mutant),]
sorttt<-tgc[order(tgc$Mutant),]
sorttt
install.packages('gtools')
install.packages('gtools')
library(gtools)
sorttt<-tgc[mixedsort(tgc$Mutant),]
sorttt
sorttt<-tgc[mixedsort(tgc$Avg),]
sorttt
sorttt<-tgc[mixedsort(tgc$Mutant),]
sorttt
library("gridExtra")
install.packages("cowplot")
library("cowplot")
 ggplot(tgc, aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0)
tgc
r1<-tgc[1]
r1
r1<-tgc[1,]
r1
r1<-tgc[2,]
r1
wt<-tgc[11,]
wt
ggplot(tgc, aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0)
ggplot(wt, aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0)
ggplot(wt,r1 aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0)
ggplot(c(wt,r1) aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0)
ggplot(c(wt,r1), aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0)
r1
wt
d<-merge(wt, r1)
d
d<-rbind(wt, r1)
d
ggplot(d, aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0)
 ggplot(tgc, aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0)
 ggplot(tgc, aes(x=Mutant, y=Avg, fill=x)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0)
 ggplot(tgc, aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0)
 ggplot(tgc, aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + opts (axis.ticks=theme_blank())
 ggplot(tgc, aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme(panel.background = element_rect(colour = "pink"))
 ggplot(tgc, aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme(panel.background = element_rect(colour = "pink"))
 ggplot(tgc, aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw()
 ggplot(tgc, aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ymax=100
 ggplot(tgc, aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,100)
wt<-rbind(tgc[11,], tgc[1,], tgc[2,])
wt
tgc
wt<-rbind(tgc[11,], tgc[1,], tgc[3,])
p1<- ggplot(wt, aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,100)
wt
p1
bf2ala<-rbind(tgc[4,], tgc[5,], tgc[6,])
bf2ala
p2<- ggplot(bf2ala, aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,100)
p2
hf2gln<-rbind(tgc[6,], tgc[7,], tgc[8,])
hf2gln
hf2gln<-rbind(tgc[7,], tgc[8,], tgc[9,])
mul<-
p3<- ggplot(hf2gln, aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,100)
p3
mut<-rbind(tgc[10,], tgc[2,])
mut
p4<- ggplot(mut, aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,100)
p4
multiplot(p1, p2, p3, p4, cols=2)
library(grid)
multiplot(p1, p2, p3, p4, cols=2)
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }
 if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
multiplot(p1, p2, p3, p4, cols=2)
multiplot(p1, p2, p3, p4, cols=1)
multiplot(p1, p2, p3, p4, cols=4)
p4<- ggplot(mut, aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90)
multiplot(p1, p2, p3, p4, cols=4)
savehistory("barplot")
p1<- ggplot(wt, aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90)
p2<- ggplot(bf2ala, aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90)
p3<- ggplot(hf2gln, aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90)
p4<- ggplot(mut, aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90)
multiplot(p1, p2, p3, p4, cols=4)
p2<- ggplot(bf2ala) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90)
p2
p2<- ggplot(bf2ala, aes(x=Mutant)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90)
p2
p2<- ggplot(bf2ala, aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90)
p2
p2<- ggplot(bf2ala, aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="bin") + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90)
p2
p2<- ggplot(bf2ala, aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity") + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90)
p2
p2<- ggplot(bf2ala, aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity", width=.5) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90)
p2
p3<- ggplot(hf2gln, aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity", width=.5) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90)
p1<- ggplot(wt, aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity", width=.5) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90)
p1
p3
p4<- ggplot(mut, aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity", width=.5) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90)
p4
p4<- ggplot(mut, aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity", width=.5) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90)
p4
multiplot(p1, p2, p3, p4, cols=4)
p1<- ggplot(wt, aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity", width=.5) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + ggtitle("WT with Caps")
p1
p1<- ggplot(wt, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.5) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + ggtitle("WT with Caps")
p1
p1<- ggplot(wt, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.5) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + ggtitle("WT with Caps") + scale_fill_discrete(name="Experimental\nCondition",
                         breaks=c("ctrl", "trt1", "trt2"),
                         labels=c("Control", "Treatment 1", "Treatment 2"))
p1
p1<- ggplot(wt, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.5) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + ggtitle("WT with Caps") + scale_fill_discrete(name="Mutants", breaks=c("M1", "M2", M3), labels=c("WT-Cap1", "WT-Cap2", "WT"))
p1<- ggplot(wt, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.5) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + ggtitle("WT with Caps") + scale_fill_discrete(name="Mutants", breaks=c("M1", "M2", "M3"), labels=c("WT-Cap1", "WT-Cap2", "WT"))
p1
p1<- ggplot(wt, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.5) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + ggtitle("WT with Caps") + scale_fill_discrete(name="Mutants", breaks=c("M1", "M2", "WT"), labels=c("WT-Cap1", "WT-Cap2", "WT"))
p1
p2<- ggplot(bf2ala, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.5) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + scale_fill_discrete(name="Mutants", breaks=c("M3", "M4", "M5"), labels=c("Y150A", "M154A", "M3+M4"))
p2
p3<- ggplot(hf2gln, aes(x=Mutant, y=Avg)) + geom_bar(position=position_dodge(), stat="identity", width=.5) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + scale_fill_discrete(name="Mutants", breaks=c("M6", "M7", "M8"), labels=c("Y150Q", "M154Q", "M6+M7"))
p3
p3<- ggplot(hf2gln, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.5) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + scale_fill_discrete(name="Mutants", breaks=c("M6", "M7", "M8"), labels=c("Y150Q", "M154Q", "M6+M7"))
p3
p4<- ggplot(mut, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.5) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + scale_fill_discrete(name="Mutants", breaks=c("M9", "M10"), labels=c("M154A+Cap1", "M154A+Cap2"))
p4
multiplot(p1, p2, p3, p4, cols=4)
p2<- ggplot(bf2ala, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.5) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + scale_fill_discrete(name="Mutants", breaks=c("M3", "M4", "M5"), labels=c("Y150A", "M154A", "M3+M4")) + ggtitle("Alanine Mutations")
p2
p3<- ggplot(hf2gln, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.5) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + scale_fill_discrete(name="Mutants", breaks=c("M6", "M7", "M8"), labels=c("Y150Q", "M154Q", "M6+M7")) + ggtitle("Glutamine Mutations")
p3
p4<- ggplot(mut, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.5) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + scale_fill_discrete(name="Mutants", breaks=c("M9", "M10"), labels=c("M154A+Cap1", "M154A+Cap2")) + ggtitle("M4 with Caps")
p4
p3
p4<- ggplot(mut, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.5) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + scale_fill_manual(values=c("salmon", "green3"),name="Mutants", breaks=c("M9", "M10"), labels=c("M154A+Cap1", "M154A+Cap2")) + ggtitle("M4 with Caps")
p4
p3
p4<- ggplot(mut, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.5) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + scale_fill_manual(values=c("salmon", "chartreuse3"),name="Mutants", breaks=c("M9", "M10"), labels=c("M154A+Cap1", "M154A+Cap2")) + ggtitle("M4 with Caps")
p4
p3
p4<- ggplot(mut, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.5) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + scale_fill_manual(values=c("salmon", "mediumseagreen"),name="Mutants", breaks=c("M9", "M10"), labels=c("M154A+Cap1", "M154A+Cap2")) + ggtitle("M4 with Caps")
p4
multiplot(p1, p2, p3, p4, cols=4)
p4<- ggplot(mut, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.5) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + scale_fill_manual(values=c("salmon", "chartreuse3"),name="Mutants", breaks=c("M9", "M10"), labels=c("M154A+Cap1", "M154A+Cap2")) + ggtitle("M4 with Caps")
multiplot(p1, p2, p3, p4, cols=4)
image<-multiplot(p1, p2, p3, p4, cols=4)
save.image(image)
save.image("image")
image
image
multiplot(p1, p2, p3, p4, cols=4)
jpeg("2W9E-Mutant")
multiplot(p1, p2, p3, p4, cols=4)
jpeg("2W9E-Mutant.jpg")
multiplot(p1, p2, p3, p4, cols=4)
dev.off
jpeg('2W9EMutant.jpg')
multiplot(p1, p2, p3, p4, cols=4)
dev.off()
multiplot(p1, p2, p3, p4, cols=4)
dev.off()
multiplot(p1, p2, p3, p4, cols=2)
dev.off()
postscript("2W9EMutant.ps")
multiplot(p1, p2, p3, p4, cols=2)
dev.off()
multiplot(p1, p2, p3, p4, cols=4)
postscript("2W9EMutant.ps")
multiplot(p1, p2, p3, p4, cols=4)
dev.off()
p4<- ggplot(mut, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.5) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + scale_fill_manual(values=c("salmon", "chartreuse3"),name="Mutants", breaks=c("M9", "M10"), labels=c("M154A+Cap1", "M154A+Cap2")) + ggtitle("M4 with Caps")
p4
p4<- ggplot(mut, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.5) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + scale_fill_manual(values=c("salmon", "chartreuse3"),name="Mutants", breaks=c("M9", "M10"), labels=c("M154A+Cap1", "M154A+Cap2")) + ggtitle("M4 with Caps") + theme(legend.position="top")
p4
p3<- ggplot(hf2gln, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.5) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + scale_fill_discrete(name="Mutants", breaks=c("M6", "M7", "M8"), labels=c("Y150Q", "M154Q", "M6+M7")) + ggtitle("Glutamine Mutations") + theme(legend.position="top")
p3
p3<- ggplot(hf2gln, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.5) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + scale_fill_discrete(name="Mutants", breaks=c("M6", "M7", "M8"), labels=c("Y150Q", "M154Q", "M6+M7")) + ggtitle("Glutamine Mutations") + theme(legend.position="top")
p2
p2<- ggplot(bf2ala, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.5) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + scale_fill_discrete(name="Mutants", breaks=c("M3", "M4", "M5"), labels=c("Y150A", "M154A", "M3+M4")) + ggtitle("Alanine Mutations") + theme(legend.position="top")
p2
p1<- ggplot(wt, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.5) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + ggtitle("WT with Caps") + scale_fill_discrete(name="Mutants", breaks=c("M1", "M2", "WT"), labels=c("WT-Cap1", "WT-Cap2", "WT")) + theme(legend.position="top")
p1
postscript("2W9EMutant.ps")
multiplot(p1, p2, p3, p4, cols=4)
dev.off()
p1<- ggplot(wt, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=1.5) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + ggtitle("WT with Caps") + scale_fill_discrete(name="Mutants", breaks=c("M1", "M2", "WT"), labels=c("WT-Cap1", "WT-Cap2", "WT")) + theme(legend.position="top")
p1
p1<- ggplot(wt, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=1.0) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + ggtitle("WT with Caps") + scale_fill_discrete(name="Mutants", breaks=c("M1", "M2", "WT"), labels=c("WT-Cap1", "WT-Cap2", "WT")) + theme(legend.position="top")
p1
p1<- ggplot(wt, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=0.8) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + ggtitle("WT with Caps") + scale_fill_discrete(name="Mutants", breaks=c("M1", "M2", "WT"), labels=c("WT-Cap1", "WT-Cap2", "WT")) + theme(legend.position="top")
p1
p1<- ggplot(wt, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(0.9), stat="identity", width=0.5) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + ggtitle("WT with Caps") + scale_fill_discrete(name="Mutants", breaks=c("M1", "M2", "WT"), labels=c("WT-Cap1", "WT-Cap2", "WT"))
p1
p1<- ggplot(wt, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(0.5), stat="identity", width=0.5) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + ggtitle("WT with Caps") + scale_fill_discrete(name="Mutants", breaks=c("M1", "M2", "WT"), labels=c("WT-Cap1", "WT-Cap2", "WT")) + theme(legend.position="top")
p1
p1<- ggplot(wt, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(0.5), stat="identity", width=0.5, binwidth=0) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + ggtitle("WT with Caps") + scale_fill_discrete(name="Mutants", breaks=c("M1", "M2", "WT"), labels=c("WT-Cap1", "WT-Cap2", "WT")) + theme(legend.position="top")
p1<- ggplot(wt, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(0.5), stat="identity", width=0.5, binwidth=0) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + ggtitle("WT with Caps") + scale_fill_discrete(name="Mutants", breaks=c("M1", "M2", "WT"), labels=c("WT-Cap1", "WT-Cap2", "WT")) + theme(legend.position="top")
p1<- ggplot(wt, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(1.0), stat="identity", width=0.5, binwidth=0) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + ggtitle("WT with Caps") + scale_fill_discrete(name="Mutants", breaks=c("M1", "M2", "WT"), labels=c("WT-Cap1", "WT-Cap2", "WT")) + theme(legend.position="top")
p1<- ggplot(wt, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(1.0), stat="identity", width=0.5) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + ggtitle("WT with Caps") + scale_fill_discrete(name="Mutants", breaks=c("M1", "M2", "WT"), labels=c("WT-Cap1", "WT-Cap2", "WT")) + theme(legend.position="top")
p1
p1
p1<- ggplot(wt, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(0.2), stat="identity", width=0.5) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + ggtitle("WT with Caps") + scale_fill_discrete(name="Mutants", breaks=c("M1", "M2", "WT"), labels=c("WT-Cap1", "WT-Cap2", "WT")) + theme(legend.position="top")
p1
p1<- ggplot(wt, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(0.2), stat="identity", width=0.9) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + ggtitle("WT with Caps") + scale_fill_discrete(name="Mutants", breaks=c("M1", "M2", "WT"), labels=c("WT-Cap1", "WT-Cap2", "WT")) + theme(legend.position="top")
p1
p2<- ggplot(bf2ala, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.9) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + scale_fill_discrete(name="Mutants", breaks=c("M3", "M4", "M5"), labels=c("Y150A", "M154A", "M3+M4")) + ggtitle("Alanine Mutations") + theme(legend.position="top")
p2
p3<- ggplot(hf2gln, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.9) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + scale_fill_discrete(name="Mutants", breaks=c("M6", "M7", "M8"), labels=c("Y150Q", "M154Q", "M6+M7")) + ggtitle("Glutamine Mutations") + theme(legend.position="top")
p4<- ggplot(mut, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.9) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + scale_fill_manual(values=c("salmon", "chartreuse3"),name="Mutants", breaks=c("M9", "M10"), labels=c("M154A+Cap1", "M154A+Cap2")) + ggtitle("M4 with Caps") + theme(legend.position="top")
postscript("2W9EMutant.ps")
multiplot(p1, p2, p3, p4, cols=4)
dev.off()
p4<- ggplot(mut, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.9) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + scale_fill_manual(values=c("salmon", "chartreuse3"), breaks=c("M9", "M10"), labels=c("M154A+Cap1", "M154A+Cap2")) + ggtitle("M4 with Caps") + theme(legend.position="top")
postscript("2W9EMutant.ps")
multiplot(p1, p2, p3, p4, cols=4)
dev.off()
p1<- ggplot(wt, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(0.2), stat="identity", width=0.5) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + ggtitle("WT with Caps") + scale_fill_discrete(breaks=c("M1", "M2", "WT"), labels=c("WT-Cap1", "WT-Cap2", "WT")) + theme(legend.position="top")
p1
postscript("2W9EMutant.ps")
multiplot(p1, p2, p3, p4, cols=4)
dev.off()
postscript("2W9EMutant.ps")
multiplot(p1, p2, p3, p4, cols=4)
dev.off()
postscript("2W9EMutant.ps")
multiplot(p1, p2, p3, p4, cols=2)
dev.off()
p1<- ggplot(wt, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(0.2), stat="identity", width=0.8) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + ggtitle("WT with Caps") + scale_fill_discrete(breaks=c("M1", "M2", "WT"), labels=c("WT-Cap1", "WT-Cap2", "WT")) + theme(legend.position="top")
postscript("2W9EMutant.ps")
multiplot(p1, p2, p3, p4, cols=2)
dev.off()
p1<- ggplot(wt, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(0.2), stat="identity", width=0.8) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + ggtitle("WT with Caps") + scale_fill_discrete(breaks=c("M1", "M2", "WT"), labels=c("WT-Cap1", "WT-Cap2", "WT"))
postscript("2W9EMutant.ps")
multiplot(p1, p2, p3, p4, cols=2)
dev.off()
p4<- ggplot(mut, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.9) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + scale_fill_manual(values=c("salmon", "chartreuse3"), breaks=c("M9", "M10"), labels=c("M154A+Cap1", "M154A+Cap2")) + ggtitle("M4 with Caps")
p2<- ggplot(bf2ala, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.5) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + scale_fill_discrete(name="Mutants", breaks=c("M3", "M4", "M5"), labels=c("Y150A", "M154A", "M3+M4")) + ggtitle("Alanine Mutations") 
savehistory("barPlot.R")
p1<- ggplot(wt, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(0.2), stat="identity", width=0.9) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + ggtitle("WT with Caps") + scale_fill_discrete(name="Mutants", breaks=c("M1", "M2", "WT"), labels=c("WT-Cap1", "WT-Cap2", "WT"))
p2<- ggplot(bf2ala, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.9) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + scale_fill_discrete(name="Mutants", breaks=c("M3", "M4", "M5"), labels=c("Y150A", "M154A", "M3+M4")) + ggtitle("Alanine Mutations")
p3<- ggplot(hf2gln, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.9) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + scale_fill_discrete(name="Mutants", breaks=c("M6", "M7", "M8"), labels=c("Y150Q", "M154Q", "M6+M7")) + ggtitle("Glutamine Mutations")
p4<- ggplot(mut, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.9) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + scale_fill_manual(values=c("salmon", "chartreuse3"),name="Mutants", breaks=c("M9", "M10"), labels=c("M154A+Cap1", "M154A+Cap2")) + ggtitle("M4 with Caps") 
postscript("2W9EMutant.ps")
multiplot(p1, p2, p3, p4, cols=2)
dev.off()
p4<- ggplot(mut, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.9) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + scale_fill_manual(values=c("salmon", "chartreuse3"),name="Mutants", breaks=c("M9", "M10"), labels=c("M154A+Cap1", "M154A+Cap2")) + ggtitle("M4 with Caps") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p4
p4<- ggplot(mut, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.9) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), element_text(angle = 90, hjust = 1), vjust=0) + theme_bw() + ylim(0,90) + scale_fill_manual(values=c("salmon", "chartreuse3"),name="Mutants", breaks=c("M9", "M10"), labels=c("M154A+Cap1", "M154A+Cap2")) + ggtitle("M4 with Caps")
p4<- ggplot(mut, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.9) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0) + theme_bw() + ylim(0,90) + scale_fill_manual(values=c("salmon", "chartreuse3"),name="Mutants", breaks=c("M9", "M10"), labels=c("M154A+Cap1", "M154A+Cap2")) + ggtitle("M4 with Caps")
p4<- ggplot(mut, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.9) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), angle=30, vjust=0) + theme_bw() + ylim(0,90) + scale_fill_manual(values=c("salmon", "chartreuse3"),name="Mutants", breaks=c("M9", "M10"), labels=c("M154A+Cap1", "M154A+Cap2")) + ggtitle("M4 with Caps")
p4
p3<- ggplot(hf2gln, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.9) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), angle=30, vjust=0) + theme_bw() + ylim(0,90) + scale_fill_discrete(name="Mutants", breaks=c("M6", "M7", "M8"), labels=c("Y150Q", "M154Q", "M6+M7")) + ggtitle("Glutamine Mutations")
p2<- ggplot(bf2ala, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.9) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), angle=30, vjust=0) + theme_bw() + ylim(0,90) + scale_fill_discrete(name="Mutants", breaks=c("M3", "M4", "M5"), labels=c("Y150A", "M154A", "M3+M4")) + ggtitle("Alanine Mutations")
p1<- ggplot(wt, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(0.2), stat="identity", width=0.9) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=0, angle=30) + theme_bw() + ylim(0,90) + ggtitle("WT with Caps") + scale_fill_discrete(name="Mutants", breaks=c("M1", "M2", "WT"), labels=c("WT-Cap1", "WT-Cap2", "WT"))
postscript("2W9EMutant.ps")
multiplot(p1, p2, p3, p4, cols=2)
dev.off()
p1<- ggplot(wt, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(0.2), stat="identity", width=0.9) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=-10, angle=30) + theme_bw() + ylim(0,90) + ggtitle("WT with Caps") + scale_fill_discrete(name="Mutants", breaks=c("M1", "M2", "WT"), labels=c("WT-Cap1", "WT-Cap2", "WT"))
p1
p1<- ggplot(wt, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(0.2), stat="identity", width=0.9) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=-1, angle=30) + theme_bw() + ylim(0,90) + ggtitle("WT with Caps") + scale_fill_discrete(name="Mutants", breaks=c("M1", "M2", "WT"), labels=c("WT-Cap1", "WT-Cap2", "WT"))
p1
p1<- ggplot(wt, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(0.2), stat="identity", width=0.9) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=-2, angle=30) + theme_bw() + ylim(0,90) + ggtitle("WT with Caps") + scale_fill_discrete(name="Mutants", breaks=c("M1", "M2", "WT"), labels=c("WT-Cap1", "WT-Cap2", "WT"))
p1
p2<- ggplot(bf2ala, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.9) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), angle=30, vjust=-2) + theme_bw() + ylim(0,90) + scale_fill_discrete(name="Mutants", breaks=c("M3", "M4", "M5"), labels=c("Y150A", "M154A", "M3+M4")) + ggtitle("Alanine Mutations")
p2
p3<- ggplot(hf2gln, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.9) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), angle=30, vjust=-2) + theme_bw() + ylim(0,90) + scale_fill_discrete(name="Mutants", breaks=c("M6", "M7", "M8"), labels=c("Y150Q", "M154Q", "M6+M7")) + ggtitle("Glutamine Mutations")
p4<- ggplot(mut, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.9) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), angle=30, vjust=-2) + theme_bw() + ylim(0,90) + scale_fill_manual(values=c("salmon", "chartreuse3"),name="Mutants", breaks=c("M9", "M10"), labels=c("M154A+Cap1", "M154A+Cap2")) + ggtitle("M4 with Caps")
postscript("2W9EMutant.ps")
multiplot(p1, p2, p3, p4, cols=2)
dev.off()
aa<-read.csv(file="testLine.csv", head=TRUE, sep=",")
aa
p1
dataSum222 <- summarySE(data, measurevar="time", groupvars=c("AA","Mutant"))
dataSum222 <- summarySE(aa, measurevar="time", groupvars=c("AA","Mutant"))
dataSum222
wtaa<-subset(dataSum, Mutant=="M1"| Mutant=="WT" | Mutant=="M2")
wtaa<-subset(dataSum222, Mutant=="M1"| Mutant=="WT" | Mutant=="M2")
wtaa
p11<-ggplot(wtaa, aes(x=AA, colour=Mutant, group=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1) + geom_line(aes(y=time)) + ggtitle("WT with Caps") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p11
multiplot(p1, p11, cols=2)
p11<-ggplot(wtaa, aes(x=AA, colour=Mutant, group=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1) + geom_line(aes(y=time)) + ggtitle("WT with Caps") + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none")
p11
multiplot(p1, p11, cols=2)
p11<-ggplot(wtaa, aes(x=AA, colour=Mutant, group=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1) + geom_line(aes(y=time)) + ggtitle("WT with Caps") + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none") + theme_bw()
p11
p11
p11<-ggplot(wtaa, aes(x=AA, colour=Mutant, group=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1) + geom_line(aes(y=time)) + ggtitle("WT with Caps") + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none")
p11
multiplot(p1, p11, cols=2)
postscript("saba2.ps")
multiplot(p1, p11, cols=2)
dev.off()
aa<-read.csv(file="testLine.csv", head=TRUE, sep=",")
aa
wtaa<-subset(dataSum222, Mutant=="M1"| Mutant=="WT" | Mutant=="M2")
p11<-ggplot(wtaa, aes(x=AA, colour=Mutant, group=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1) + geom_line(aes(y=time)) + ggtitle("WT with Caps") + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none")
p11
gro2<-subset(dataSum222, Mutant=="M3"| Mutant=="M4" | Mutant=="M4")
p22<-ggplot(gro2, aes(x=AA, colour=Mutant, group=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1) + geom_line(aes(y=time)) + ggtitle("WT with Caps") + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none")
p22
gro2<-subset(dataSum222, Mutant=="M3"| Mutant=="M4" | Mutant=="M5")
p22<-ggplot(gro2, aes(x=AA, colour=Mutant, group=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1) + geom_line(aes(y=time)) + ggtitle("WT with Caps") + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none")
p22
p11
gro
gro2
gro2<-subset(dataSum222, Mutant=="M3"| Mutant=="M4" | Mutant=="M5")
gro2
gro3<-subset(dataSum222, Mutant=="M3"| Mutant=="M5" | Mutant=="M6")
gro2
dataSum222
aa<-read.csv(file="testLine.csv", head=TRUE, sep=",")
dataSum222 <- summarySE(aa, measurevar="time", groupvars=c("AA","Mutant"))
dataSum222
gro2<-subset(dataSum222, Mutant=="M3"| Mutant=="M4" | Mutant=="M5")
gro2
p22<-ggplot(gro2, aes(x=AA, colour=Mutant, group=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1) + geom_line(aes(y=time)) + ggtitle("WT with Caps") + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none")
p22
gro2<-subset(dataSum222, Mutant=="M3"| Mutant=="M4" | Mutant=="M5")
gro2
gro2<-subset(dataSum222, Mutant=="M3"| Mutant=="M4" | Mutant=="M5")
p22<-ggplot(gro2, aes(x=AA, colour=Mutant, group=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1) + geom_line(aes(y=time)) + ggtitle("WT with Caps") + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none")
p22
gro3<-subset(dataSum222, Mutant=="M6"| Mutant=="M7" | Mutant=="M8")
p33<-ggplot(gro3, aes(x=AA, colour=Mutant, group=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1) + geom_line(aes(y=time)) + ggtitle("WT with Caps") + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none")
p33
p33
gro4<-subset(dataSum222, Mutant=="M9"| Mutant=="M10")
gro4
p44<-ggplot(gro4, aes(x=AA, colour=Mutant, group=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1) + geom_line(aes(y=time)) + ggtitle("WT with Caps") + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none")
p44
p22<-ggplot(gro2, aes(x=AA, colour=Mutant, group=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1) + geom_line(aes(y=time)) + ggtitle("Alanine Mutations") + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none")
p33<-ggplot(gro3, aes(x=AA, colour=Mutant, group=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1) + geom_line(aes(y=time)) + ggtitle("Glutamin Mutations") + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none")
p44<-ggplot(gro4, aes(x=AA, colour=Mutant, group=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1) + geom_line(aes(y=time)) + ggtitle("M4 with Caps") + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none")
multiplot(p1, p11, p2, p22, p3, p33, p4, p44, col=2)
multiplot(p1, p2, p3, p4, p11, p22, p33, p44, col=2)
multiplot(p1, p11, cols=2)
multiplot(p1, p2, p3, p4, p11, p22, p33, p44, cols=2)
postscript("saba22.ps")
multiplot(p1, p2, p3, p4, p11, p22, p33, p44, cols=2)
dev.off()
multiplot(p1, p2, p3, p4, p11, p22, p33, p44, cols=2)
savehistory("combinedPlot.R")
