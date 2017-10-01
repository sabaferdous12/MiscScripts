################################################################################
# Version:	V1.1
# Date:		18.02.2015
# Author:	Avneet Saini
# Description:	Plot upto five phaistos observables as line graphs in one image
#
# Usage:        ./usr/bin/Rscript plotPhaistos.R <name1> <name2> ...
#
# Example:	for input files: name1.dat, name2.dat, name3.dat
#		./usr/bin/Rscript plotPhaistos.R name1 name2 name3
#
# Output:	Three files are created corresponding to energy,rg and helix
# 		content in both PNG and PDF format. The output file name
#		corresponds to the command line arguments joined together
#		seperated by '-' followed by '-energy' or '-rg' or '-hx'.       
#		eg:name1-name2-name3-energy.png or
#             	name1-name2-name3-rg.pdf 
################################################################################

# Reads the phaistos observables' files (*.dat) each of which contains 4
# columns: id, @energy-sum, rg and helix-content. It takes the mean fo every
# 1000 entries and creates line graphs for maximum of 5 input files. 

# colect arguments
args <- commandArgs(TRUE)

## if no argument is passed or argument number is more than 5, display help
l <-length(args)
if(l<1 | l>5){      
	args<- c("--help")
	}	       

# help section
if ("--help" %in% args) {
cat("
	Description:
	This script is used to plot upto five phaistos observables as line
	graphs in one image. It reads the phaistos observables' files (*.dat)
	specified on the command line by their basenames. Each file contains
	4 columns: id, @energy-sum, rg and helix-content. It takes the mean for
	every 1000 entries and creates line graphs for upto 5 input files. 

	Usage:
        ./usr/bin/Rscript plotPhaistos.R <name1> <name2> ...
	(Give name of phaistos file without extension
	Example:
	For input files: name1.dat, name2.dat, name3.dat
	./usr/bin/Rscript plotPhaistos.R name1 name2 name3

	
   	--help	ask for help
")
	q(save="no")
}





################################################################################
######################### The proper code ######################################

# load libraries
library("ggplot2")
library("gridExtra")
library("reshape2")


# Output filename (basename)
outfilename <- paste(args, collapse="-")

### Function to get plotting options from user ##########
get.It <- function(x){
	con <- file(x)
	on.exit(close(con))
	selection <- readLines(con, n = 1L)
	selection
}		


# Prompt user to define color panel
cat("Select a color panel for your graphs:    \
    Your options: 1 or 2                       \
    Panel 1 = red, blue, green, orange, pink   \
    Panel 2 = yellow, blue, orange, green, pink\n"
    )
option <- "stdin"
panel <- get.It(option)

################ Function to get color panel for plots ###################
get.colors <- function(p){
	choice <- c()
	# Specify the sets of colour vectors 
	colors1 <- c("red", "blue", "green", "orange", "pink")
	colors2 <- c("#E69F00", "#0072B2", "#D55E00", "#009E73", "#CC79A7")
	if (p == 1) choice <- colors1
	if (p == 2) choice <- colors2
	return (choice)
}

# colors that will be used while plotting
colors <- get.colors(panel)


# check if file corresponding to the argument passed exists
inputFiles <- c()

for (z in 1:length(args)) {
   	filebasename <- args[z]
   	filename <- paste(filebasename,".dat",sep="")
   	if (file.exists(filename)){
      		inputFiles <- c(inputFiles, filename)
   	}		
}

# count of existing input files 
count <- length(inputFiles)

### Function Read file and load table#########
load.file <- function(filename) {
    	fileTable <- read.table(filename, comment.char = "#",header = F,
			 sep = "", quote= "")
    	fileTable
}

# Read input files and load table
dataTable <- lapply(inputFiles, load.file)

##### Function to calculate mean of every 1000 entries######
compute.mean <- function(data) {
	# create vector for energy, rg and helix content
    	energy <- data$V2
    	rg <- data$V3
    	helix <- data$V4

    	# Create empty columns for iteration number, mean of energy, mean rg and
    	# mean helix content
    	data$V5 <- NA    
    	data$V6 <- NA    
    	data$V7 <- NA 
    	data$V8 <- NA
    
    	# Group every 1000 values to take the mean
    	j <- 1
    	for(i in seq(1,nrow(data),1000)){ 
    		# create a subset with the values 
    		subsetEnergy <- energy[c(i:(i+999))]
    		subsetRg <- rg[c(i:(i+999))]
    		subsetHelix <- helix[c(i:(i+999))]

    	# Iteration number: each line in the phaistos output corresponds to 
	# 5000th iteration. And we are calculating mean of every 1000 entries.
	# So the iteration number for the mean values is j*5000*1000=j*5000000 
    	data[j,]$V5 <- (j*5000000)
    	# save the mean in position i,j of the third column
    	data[j,]$V6 <- mean(subsetEnergy)
    	data[j,]$V7 <- mean(subsetRg)
    	data[j,]$V8 <- mean(subsetHelix)
    	# increment j
    	j <- j+1
    	}	
    	filedata.frame <- na.omit(data)
    	filedata.frame   
}

# Compute Mean
finalTable <- lapply(dataTable, compute.mean)
   
##### Function to plot Mean energies #########################
energy.plot <- function(a,b,o,c){
	# define column of iteration number
	iteration<-b[[1]][5]
    	# create a dataframe
    	df.E <- data.frame(iteration)

    	# iterate over the number of files set for plotting
    	for (i in 1:a){
        	#create a subset for the cols specifying mean energies
        	sub.E <- b[[i]][6]
    
        	# bind the energy cols to the dataframe
        	df.E <- cbind(df.E, sub.E)
        	i <- i+1
    	}
    	# change all colnames
    	if (a ==1){ colnames(df.E) <- c("it", "1")}
    	if (a ==2){ colnames(df.E) <- c("it", "1", "2")}
    	if (a ==3){ colnames(df.E) <- c("it", "1", "2", "3")}
    	if (a ==4){ colnames(df.E) <- c("it", "1", "2", "3", "4")}
    	if (a ==5){ colnames(df.E) <- c("it", "1", "2", "3", "4", "5")}
    	df.E

    	# Specify legend label vector
     	list <- c("Energy1", "Energy2", "Energy3", "Energy4", "Energy5")
    
    	df.E_long <- melt(df.E, id="it")  # convert to long format
    
    	# Plot
    	pE <- ggplot(data=df.E_long, aes(x=it, y=value, colour=variable)) + 
    		geom_line()
    
    	# Change range of Y axis, and set manual colors
    	# and label the axis
    	pE + scale_y_continuous(limit = c(0, 400))  +
   		scale_colour_manual(values = c, labels = list) + 
    		xlab("Iteration") + ylab("Energy") 

    	# To Change range of Y axis
   	# pE + scale_y_continuous(limit = c(0, 500))  
                    
	# Save the plot as image in PNG & PDF formats with following attributes
    	ggsave(paste0(o,"-energy.png", sep=""),height=5,width=10,dpi=300) 
    	ggsave(paste0(o,"-energy.pdf", sep=""),height=5,width=10)
}  

energy.plot(count, finalTable, outfilename, colors)

######### Function to plot Mean RG #########################
rg.plot <- function(a,b,o,c){
	# define column of iteration number
	iteration<-b[[1]][5]
	# create a dataframe
	df.Rg <- data.frame(iteration)
    	# iterate over the number of files set for plotting
    	for (i in 1:a){
    		# subset for Rg
    		sub.Rg <- b[[i]][7]
    		# bind cols
    		df.Rg <- cbind(df.Rg, sub.Rg)
    		i <- i+1
    	}		
    	# change all colnames
    	if (a ==1){ colnames(df.Rg) <- c("it", "1")}
    	if (a ==2){ colnames(df.Rg) <- c("it", "1", "2")}
    	if (a ==3){ colnames(df.Rg) <- c("it", "1", "2", "3")}
    	if (a ==4){ colnames(df.Rg) <- c("it", "1", "2", "3", "4")}
    	if (a ==5){ colnames(df.Rg) <- c("it", "1", "2", "3", "4", "5")}
    	df.Rg

    	#Specify legend label vector
    	list <- c("RG1", "RG2", "RG3", "RG4", "RG5")
    

    	df.Rg_long <- melt(df.Rg, id="it")  # convert to long format
    	# Plot
    	pR <- ggplot(data=df.Rg_long, aes(x=it,y=value, colour=variable)) +
    		geom_line()
    	# set manual colors and label the axis
    	pR + scale_colour_manual(values = c, labels = list) + 
    		xlab("Iteration") + ylab("Radius of Gyration")    

    	# Save a new image fileTable with following attributes
    	ggsave(paste0(o,"-rg.png", sep=""),height=5,width=10,dpi=300) 
    	ggsave(paste0(o,"-rg.pdf", sep=""),height=5,width=10)
}  

rg.plot(count, finalTable, outfilename, colors)

######### Function to plot Mean Helix Content #####################
hx.plot <- function(a,b,o,c){
	# iteration col
	iteration<-b[[1]][5]
	# New dataframe
	df.Hx <- data.frame(iteration)
    		for (i in 1:a){
    		# subset
    		sub.Hx <- b[[i]][8]
    		# bind the subsets
    		df.Hx <- cbind(df.Hx, sub.Hx)
    		i <- i+1
    	}	
    	# change all colnames
    	if (a ==1){ colnames(df.Hx) <- c("it", "1")}
    	if (a ==2){ colnames(df.Hx) <- c("it", "1", "2")}
    	if (a ==3){ colnames(df.Hx) <- c("it", "1", "2", "3")}
    	if (a ==4){ colnames(df.Hx) <- c("it", "1", "2", "3", "4")}
    	if (a ==5){ colnames(df.Hx) <- c("it", "1", "2", "3", "4", "5")}
    	df.Hx

    	# Specify legend label vector
     	list <- c("Helix1", "Helix2", "Helix3", "Helix4", "Helix5")

    	df.Hx_long <- melt(df.Hx, id="it")  # convert to long format
    	# Plot
    	pH <- ggplot(data=df.Hx_long,aes(x=it, y=value,colour=variable)) +
    		geom_line()
    	# set manual colors and label the axis
    	pH + scale_colour_manual(values = c, labels = list) + 
    		xlab("Iteration") + ylab("Helix Content")    

    	# Save a new image fileTable with following attributes
    	ggsave(paste0(o,"-hx.png", sep=""),height=5,width=10,dpi=300) 
    	ggsave(paste0(o,"-hx.pdf", sep=""),height=5,width=10)
}  

hx.plot(count, finalTable, outfilename, colors)
################################################################################
#############################~~~~~~~END~~~~~~~##################################


