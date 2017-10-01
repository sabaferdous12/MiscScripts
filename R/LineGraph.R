savehistory("SS")
data<-read.csv(file="testLine.csv", head=TRUE, sep=",")
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
dataSum <- summarySE(data, measurevar="time", groupvars=c("AA","Mutant"))
dataSum
ggplot(dataSum, aes(x=AA, colour=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1, colour="black") + geom_line(aes(y=time),group=1)
library(ggplot2)
library(ggplot2)
ggplot(dataSum, aes(x=AA, colour=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1, colour="black") + geom_line(aes(y=time),group=1)
ggplot(dataSum, aes(x=AA, colour=Mutant)) + geom_line(aes(y=time),group=1)
ggplot(dataSum, aes(x=AA, colour=Mutant)) + geom_line(aes(y=time))
ggplot(dataSum, aes(x=AA, colour=Mutant)) + geom_line(aes(y=time),group=1)
ggplot(dataSum, aes(x=AA, colour=Mutant)) + geom_bar(position=position_dodge(0.2), stat="identity", width=0.9)
ggplot(dataSum, aes(x=AA, y=time, colour=Mutant)) + geom_bar(position=position_dodge(0.2), stat="identity", width=0.9)
ggplot(dataSum, aes(x=AA, y=time)) + geom_bar(position=position_dodge(0.2), stat="identity", width=0.9)
ggplot(dataSum, aes(x=AA, y=time, fill=Mutant)) + geom_bar(position=position_dodge(0.2), stat="identity", width=0.9)
ggplot(dataSum, aes(x=AA, y=time, fill=Mutant)) + geom_bar(position=position_dodge(0.2), stat="identity", width=0.9) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1, colour="black")
postscript("WT-LineGraph.ps")
ggplot(dataSum, aes(x=AA, y=time, fill=Mutant)) + geom_bar(position=position_dodge(0.2), stat="identity", width=0.9) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1, colour="black")
dev.off()
ggplot(dataSum, aes(x=AA, colour=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1, colour="black") + geom_line(aes(y=time),group=1)
postscript("WT-LineGraph2.ps")
ggplot(dataSum, aes(x=AA, colour=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1, colour="black") + geom_line(aes(y=time),group=1)
dev.off()
ggplot(dataSum, aes(x=AA, colour=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1, colour="black") + geom_line(aes(y=time))
ggplot(dataSum, aes(x=AA, colour=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1, colour="black") + geom_line(aes(y=time),group=1)
ggplot(dataSum, aes(x=AA, colour=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1, colour="black") + geom_line(aes(y=time),group=1, position= position_dodge(0.1))
ggplot(dataSum, aes(x=AA, colour=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1, colour="black") + geom_line(aes(y=time),group=1, position= position_dodge(0.5))
ggplot(dataSum, aes(x=AA, colour=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1, colour="black") + geom_line(aes(y=time),group=1, position= position_dodge(0.9))
ggplot(dataSum, aes(x=AA, colour=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1, colour="black") + geom_line(aes(y=time),group=1, position= position_dodge(0.9))
dataSum
ggplot(dataSum, aes(x=AA, colour=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1, colour="black") + geom_line(aes(y=time), position= position_dodge(0.9))
ggplot(dataSum, aes(x=AA, colour=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1, colour="black") + geom_line(aes(y=time),group=Mutant, position= position_dodge(0.9))
ggplot(dataSum, aes(x=AA, colour=Mutant, group=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1, colour="black") + geom_line(aes(y=time), position= position_dodge(0.9))
ggplot(dataSum, aes(x=AA, colour=Mutant, group=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1, colour="black") + geom_line(aes(y=time))
ggplot(dataSum, aes(x=AA, colour=Mutant, group=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1, colour=Mutant) + geom_line(aes(y=time))
ggplot(dataSum, aes(x=AA, colour=Mutant, group=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1, colour="Mutant") + geom_line(aes(y=time))
ggplot(dataSum, aes(x=AA, colour=Mutant, group=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1, colour=group) + geom_line(aes(y=time))
ggplot(dataSum, aes(x=AA, colour=Mutant, group=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1, colour="black") + geom_line(aes(y=time))
ggplot(dataSum, aes(x=AA, colour=Mutant, group=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1) + geom_line(aes(y=time))
ggplot(dataSum, aes(x=AA, colour=Mutant, group=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1) + geom_line(aes(y=time)) + ggtitle("WT with Caps")
ggplot(dataSum, aes(x=AA, colour=Mutant, group=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1) + geom_line(aes(y=time)) + ggtitle("WT with Caps") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(dataSum, aes(x=AA, colour=Mutant, group=Mutant)) + geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1) + geom_line(aes(y=time)) + ggtitle("WT with Caps") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
savehistory("LineGraph.R")
