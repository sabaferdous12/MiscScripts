args <- (commandArgs(T))

library(ggplot2)

# function that replaces all @ with # given a file
editXVGfile <- function(input) {
  for(f in input) {
    y <- gsub("@","#",readLines(f))
    cat(y,file=f,sep="\n") 
  }
}

editXVGfile(paste(args[1]))

omega.angle.file <- read.table(paste(args[1]),header=F,comment.char="#",sep="",quote="")

omega.ggplot <- ggplot(omega.angle.file,aes(x=omega.angle.file$V1)) 
gg.omega <- omega.ggplot + geom_line(aes(y=omega.angle.file$V2), colour="black") +
            xlab("degrees") + ylab("density") + ggtitle(expression(omega))+   #(expression(omega~values)) 
      theme(legend.title=element_blank(), title=element_text(size=17),
      axis.text=element_text(size=20),
      axis.title.x = element_text(size=30),
      axis.title.y = element_text(size=30)
    )
output.omega <- paste0(paste(args[2]),".png")
ggsave(output.omega,gg.omega,height=7,width=11,dpi=300)

