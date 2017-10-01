data<-read.csv(file="2W9E_AvgSS_10Rep_2.csv", head=TRUE, sep=",")

dataSum <- summarySE(data, measurevar="Avg", groupvars=c("Mutant"))

aadata<-read.csv(file="2W9E_RES-SS_2.csv", head=TRUE, sep=",")

aadataSum <- summarySE(aadata, measurevar="time", groupvars=c("AA","Mutant"))


wt<-rbind(dataSum[1, ], dataSum[2, ], dataSum[4, ])
wt$V1<-factor(wt$Mutant, levels=unique(as.character(wt$Mutant)))

WT<-ggplot(wt, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(0.2), stat="identity", width=0.5)  + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=-2, angle=30) + ylim(0,90) + ggtitle ("2W9E WT with Cap1 and Cap2 \n 10 Replicas for 500ns ") + theme(plot.title = element_text(lineheight=.8, face="bold")) + xlab("Peptide Mutant") + ylab("Average Time")
##########
g1<-rbind(dataSum[6, ], dataSum[9, ], dataSum[11, ], dataSum[3,])
g1$Mutant<-factor(g1$Mutant, levels=unique(as.character(g1$Mutant)))

G1<-ggplot(g1, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(0.2), stat="identity")  + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=-2, angle=30) + ylim(0,90) + ggtitle ("M154 Mutant with Cap1 and Cap2 \n 10 Replicas for 500ns ") + theme(plot.title = element_text(lineheight=.8, face="bold")) + xlab("Peptide Mutant") + ylab("Average Time")

####################
aaWT<-rbind((subset(aadataSum, Mutant=="M-WT")), (subset(aadataSum, Mutant=="M1")), (subset(aadataSum, Mutant=="M2")))
aaWT$Mutant<-factor(aaWT$Mutant, levels=unique(as.character(aaWT$Mutant)))


AAWT<-ggplot(aaWT, aes(x=AA, colour=Mutant, group=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1) + geom_line(aes(y=time)) + ggtitle("2W9E WT Peptide with Cap1 and Cap2") + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none") + ylab("Average Time")
######################
aaG1<-rbind((subset(aadataSum, Mutant=="M4")), (subset(aadataSum, Mutant=="M7")), (subset(aadataSum, Mutant=="M9")), (subset(aadataSum, Mutant=="M10")))
aaG1$Mutant<-factor(aaG1$Mutant, levels=unique(as.character(aaG1$Mutant)))

AAG1<-ggplot(aaG1, aes(x=AA, colour=Mutant, group=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1) + geom_line(aes(y=time)) + ggtitle("M154 Mutant with Cap1 and Cap2") + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none") + ylab("Average Time")
######################

pdf("2W9E_500ns_2.pdf", width = 12, height= 8)
multiplot(WT, G1, AAWT, AAG1, cols=2)
dev.off()

##### Methods

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
###########


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

####
