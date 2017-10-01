initial.dir<-getwd()
setwd("/export/people/Saba/Simulation_Analysis/PeptideLinear/Mutated/2W9E/R-Plots-Data")
library(nlme)
library(ggplot2)
library(grid)

#######USAGE#####
# Rscript getBarPlots.R 2W9E_Avg_SS.csv
#################


# reading file names
args <- (commandArgs(TRUE))

for (i in 1:length(args))
{
data <- read.csv(file=args[i], head=TRUE, sep=",")
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
dataSum <- summarySE(data, measurevar="Avg", groupvars=c("Mutant"))
print(dataSum)

# Grouping WT, M1 and M2
wt<-rbind(dataSum[11,], dataSum[1,], dataSum[3,])
wt$Mutant<-factor(wt$Mutant, levels=wt$Mutant[order(wt$Avg)])

# Plotting WT group
p1<- ggplot(wt, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(0.2), stat="identity", width=0.9) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), vjust=-2, angle=30) + theme_bw() + ylim(0,90) + ggtitle("WT with Caps") + scale_fill_discrete(name="Mutants", breaks=c("M1", "M2", "WT"), labels=c("WT-Cap1", "WT-Cap2", "WT")) + ylab("Percentage of Time")

# Grouping M3, M4, M5
hf2ala<-rbind(dataSum[4,], dataSum[5,], dataSum[6,])
# Plotting Alanine mutation group
p2<- ggplot(hf2ala, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.9) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), angle=30, vjust=-2) + theme_bw() + ylim(0,90) + scale_fill_discrete(name="Mutants", breaks=c("M3", "M4", "M5"), labels=c("Y150A", "M154A", "M3+M4")) + ggtitle("Alanine Mutations") + ylab("Percentage of Time")


# Grouping M6, M7, M8
hf2gln<-rbind(dataSum[7,], dataSum[8,], dataSum[9,])
# Plotting Glutamine Mutation group
p3<- ggplot(hf2gln, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.9) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), angle=30, vjust=-2) + theme_bw() + ylim(0,90) + scale_fill_discrete(name="Mutants", breaks=c("M6", "M7", "M8"), labels=c("Y150Q", "M154Q", "M6+M7")) + ggtitle("Glutamine Mutations") + ylab("Percentage of Time")

# Grouping M9, M10
mut<-rbind(dataSum[10,], dataSum[2,])
mut$Mutant<-factor(mut$Mutant, levels=unique(as.character(mut$Mutant)))

p4<- ggplot(mut, aes(x=Mutant, y=Avg, fill=Mutant)) + geom_bar(position=position_dodge(), stat="identity", width=.9) + geom_errorbar(aes(ymin=Avg-se, ymax=Avg+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=format(round(Avg, 2), nsmall = 2)), angle=30, vjust=-2) + theme_bw() + ylim(0,90) + scale_fill_manual(values=c("salmon", "#42C642"),name="Mutants", breaks=c("M9", "M10"), labels=c("M154A+Cap1", "M154A+Cap2")) + ggtitle("M4 with Caps") + ylab("Percentage of Time")


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



# Saving post script file
postscript("saba.ps")
#tiff("saba.tiff")
multiplot(p1, p2, p3, p4, cols=2)
dev.off()

}

######### Functions ###########

