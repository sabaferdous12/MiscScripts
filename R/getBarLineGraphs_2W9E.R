initial.dir<-getwd()
setwd("/cs/ExportBackup/People/ucbterd/Simulation_Analysis/PeptideLinear/Mutated/2W9E/ss-2W9E")
library(nlme)
library(ggplot2)
library(grid)

#######USAGE#####
# Rscript getBarLineGraphs_2W9E.R 2W9E_Avg_SS.csv 2W9E_MutantAA_SS.csv
#################

# reading file names
args <- (commandArgs(TRUE))

# Reading Avg time file
data <- read.csv(file=args[1], head=TRUE, sep=",")
# Reading AA SS time file
aa<-read.csv(file=args[2], head=TRUE, sep=",")

print(data)

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
# Calculates Standard deviation. standard error and Confidence
# For Avg time
dataSum <- summarySE(data, measurevar="Avg", groupvars=c("Mutant"))
print(dataSum)
# For each AA
dataSum222 <- summarySE(aa, measurevar="time", groupvars=c("AA","Mutant"))
print (dataSum222)

# Grouping WT, M1 and M2
wt<-rbind(dataSum[11,], dataSum[1,], dataSum[3,])
wt$Mutant<-factor(wt$Mutant, levels=wt$Mutant[order(wt$Avg)])
# Plotting WT group
p1<- ggplot(wt, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(0.2), stat="identity", width=0.9) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=-0.5) + theme_bw() + ylim(0,90) + ggtitle("WT with Caps") + scale_fill_discrete(name="Mutants", breaks=c("WT", "M1", "M2"), labels=c("WT", "WT-Cap1", "WT-Cap2")) + ylab("Avg Time")

###
# Grouping WT, M1 and M2
gro1<-rbind((subset(dataSum222, Mutant=="WT")), (subset(dataSum222, Mutant=="M1")), (subset(dataSum222, Mutant=="M2")))
#gro1$Mutant<-factor(gro1$Mutant, levels=gro1$Mutant[order(gro1$time)])
gro1$Mutant<-factor(gro1$Mutant, levels=unique(as.character(gro1$Mutant)))

# Plotting WT group
p11<-ggplot(gro1, aes(x=AA, colour=Mutant, group=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1) + geom_line(aes(y=time)) + ggtitle("WT with Caps") + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none") + ylab("Avg Time")
###

# Grouping M3, M4, M5
hf2ala<-rbind(dataSum[4,], dataSum[5,], dataSum[6,])
# Plotting Alanine mutation group
p2<- ggplot(hf2ala, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.9) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=-0.5) + theme_bw() + ylim(0,90) + scale_fill_discrete(name="Mutants", breaks=c("M3", "M4", "M5"), labels=c("Y150A", "M154A", "M3+M4")) + ggtitle("Alanine Mutations") + ylab("Avg Time")

##
# Grouping M3, M4, M5
gro2<-subset(dataSum222, Mutant=="M3"| Mutant=="M4" | Mutant=="M5")
# Plotting Alanine mutation group
p22<-ggplot(gro2, aes(x=AA, colour=Mutant, group=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1) + geom_line(aes(y=time)) + ggtitle("Alanine Mutations") + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none") + ylab("Avg Time")
####

# Grouping M6, M7, M8
hf2gln<-rbind(dataSum[7,], dataSum[8,], dataSum[9,])
# Plotting Glutamine Mutation group
p3<- ggplot(hf2gln, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.9) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=-0.5) + theme_bw() + ylim(0,90) + scale_fill_discrete(name="Mutants", breaks=c("M6", "M7", "M8"), labels=c("Y150Q", "M154Q", "M6+M7")) + ggtitle("Glutamine Mutations") + ylab("Avg Time")

###
# Grouping M6, M7, M8
gro3<-subset(dataSum222, Mutant=="M6"| Mutant=="M7" | Mutant=="M8")
# Plotting Glutamine Mutation group
p33<-ggplot(gro3, aes(x=AA, colour=Mutant, group=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1) + geom_line(aes(y=time)) + ggtitle("Glutamin Mutations") + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none") + ylab("Avg Time")
###

# Grouping M9, M10
mut<-rbind(dataSum[10,], dataSum[2,])
mut$Mutant<-factor(mut$Mutant, levels=unique(as.character(mut$Mutant)))

# Plotting M4 with Caps
p4<- ggplot(mut, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.9) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=-0.5) + theme_bw() + ylim(0,90) + scale_fill_manual(values=c("salmon", "#42C642"),name="Mutants", breaks=c("M9", "M10"), labels=c("M154A+Cap1", "M154A+Cap2")) + ggtitle("M4 with Caps") + ylab("Avg Time")
###

# Grouping M9, M10
gro4<-rbind((subset(dataSum222, Mutant=="M9")), (subset(dataSum222, Mutant=="M10")))
gro4$Mutant<-factor(gro4$Mutant, levels=unique(as.character(gro4$Mutant)))
# Plotting M4 with Caps
p44<-ggplot(gro4, aes(x=AA,  colour=Mutant, group=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1) + geom_line(aes(y=time)) + scale_colour_manual(values=c("salmon", "#42C642")) + ggtitle("M4 with Caps") + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none") + ylab("Avg Time")

####
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

# In order to plot Bar plots and line graph on one page

# Saving post script file
postscript("combinedtest.ps")

multiplot(p1, p2, p3, p4, p11, p22, p33, p44, cols=2)
#ggsave("test.png", device = NULL, dpi=300)
dev.off()


