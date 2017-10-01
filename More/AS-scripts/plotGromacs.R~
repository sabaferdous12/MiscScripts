################################################################################
# Version:	    	V1.0
# Date:		    	24.02.2015
# Author:	    	Avneet Saini
# Description:		Plot line graph for upto five Gromacs *.xvg files in one
#			image
#
# Usage:        	./usr/bin/Rscript plotGromacs.R <name1> <name2> ...
#
# Example:	    	for input files: name1.xvg, name2.xvg
#		        ./usr/bin/Rscript plotGromacs.R name1 name2
#
# Output:	    	Creates a file in PNG and PDF format. The output file
#               	name corresponds to the command line arguments joined
#			together seperated by '-'. eg:name1-name2.png or
#			name1-name2.pdf 
################################################################################

# collect arguments
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
This script is used to plot upto five Gromacs observables as line graphs in one
image. It reads the gromacs *.xvg files specified on the command line by their
basenames. Be sure that the input files specified contain similar variables. 

Usage:
    ./usr/bin/Rscript plotGromacs.R <name1> <name2> ...
	(Give name of file without extension)
	Example:
	For input files: name1.xvg, name2.xvg
	./usr/bin/Rscript plotGromacs.R name1 name2 

	
   	--help	ask for help
")	
	q(save="no")
}	




################################################################################
############################ The proper code ###################################

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


# Prompt user to define what to plot
cat("What do you want to plot?\n YOUR options: \
    r       - rmsd                     \
    rg      - radius of gyration       \
    rf      - RMSF                     \
    d       - density                  \
    t       - temperature              \
    p       - pressure                 \
    e       - potential                \
    s       - secondary structure: \n"
    )
option1 <- "stdin"
option <- get.It(option1)


# Prompt user to define color panel
cat("Select a color panel for your graphs:    \
    Your options: 1 or 2                       \
    Panel 1 = red, blue, green, orange, pink   \
    Panel 2 = yellow, blue, orange, green, pink\n"
    )
option2 <- "stdin"
panel <- get.It(option2)

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

### Function to check if file corresponding to the argument passed exists ###

get.list <- function(args){
	files <- c()
	for (z in 1:length(args)) {
   		filebasename <- args[z]
   		filename <- paste(filebasename,".xvg",sep="")
   			if (file.exists(filename)){
    				files <- c(files, filename)
       				}	
	}	
	return (files)
}	

inputFiles <- get.list(args)

# count of existing input files 
count <- length(inputFiles)



### Function to edit xvg file #####################

edit.xvgFiles <- function(input){
# format the *.xvg file such that the symbol '@' is replaced with '#'
for(f in input){
x <- readLines(f)
y <- gsub( "@", "#", x )
cat(y, file=f, sep="\n")
}
}

# replace '@' with '#' in xvg files
edit.xvgFiles(inputFiles)

### Function Read file and load table #########
load.file <- function(filename) {
    fileTable <- read.table(filename, comment.char="#",header=F,sep="",quote="")
    fileTable
}

# Read input files and load table
dataTable <- lapply(inputFiles, load.file)


####### Function to get list of labels in for the plot #####################
get.List <- function(split, count){
	list <- c()
	split <-strsplit(inputFiles, "_")
	for (i in 1:count){
    		list <- c(list, split[[i]][1])
		}	
	return (list)
}	

# list of labels to be used in the graph
labels <- get.List(split, count)

######### Plot RMSD ########################################
plotRMSD <- function(a,b,o,l,c){
    # define column of time
    time<-b[[1]][1]
    # create a dataframe
    df.rmsd <- data.frame(time)

    # iterate over the number of files set for plotting
    for (i in 1:a){
        #create a subset for the cols specifying RMSD
        sub.rmsd <- b[[i]][2]
    
        # bind the energy cols to the dataframe
        df.rmsd <- cbind(df.rmsd, sub.rmsd)
        i <- i+1
	}
	# change all colnames
    	if (a ==1){ colnames(df.rmsd) <- c("time", "1")}
    	if (a ==2){ colnames(df.rmsd) <- c("time", "1", "2")}
    	if (a ==3){ colnames(df.rmsd) <- c("time", "1", "2", "3")}
    	if (a ==4){ colnames(df.rmsd) <- c("time", "1", "2", "3", "4")}
    	if (a ==5){ colnames(df.rmsd) <- c("time", "1", "2", "3", "4", "5")}
    	df.rmsd
    
	# Convert to long format
    	df.rmsd_long <- melt(df.rmsd, id="time")  
    
    	# Plot
    	pRmsd <- ggplot(data=df.rmsd_long,aes(x=time,y=value,colour=variable))+
                 geom_line()
    
    	# Set manual colors and label the axis
    	pRmsd + scale_colour_manual(values = c, labels = l) + 
    		xlab("Time (ns)") + ylab("RMSD (nm)") 

        # To Change range of Y axis
    	# pE + scale_y_continuous(limit = c(0, 500))  
                    
	# Save the plot as image in PNG & PDF formats with following attributes
    	ggsave(paste0(o,".png", sep=""),height=5,width=10,dpi=300) 
    	ggsave(paste0(o,".pdf", sep=""),height=5,width=10)
}  

# Plot RMSD
if (option == "r"){
	plotRMSD(count, dataTable, outfilename, labels, colors)
}

######### Radius of gyration #################
plotRG <- function(a,b,o,l,c){
	# define column of time
    	time<-b[[1]][1]
    	# create a dataframe
    	df.rg <- data.frame(time)

    	# iterate over the number of files set for plotting
    	for (i in 1:a){
        	#create a subset for the cols specifying RGs
        	sub.rg <- b[[i]][2]
    
        	# bind the cols to the dataframe
        	df.rg <- cbind(df.rg, sub.rg)
        	i <- i+1
    	}	
    	# change all colnames
    	if (a ==1){ colnames(df.rg) <- c("time", "1")}
    	if (a ==2){ colnames(df.rg) <- c("time", "1", "2")}
    	if (a ==3){ colnames(df.rg) <- c("time", "1", "2", "3")}
    	if (a ==4){ colnames(df.rg) <- c("time", "1", "2", "3", "4")}
    	if (a ==5){ colnames(df.rg) <- c("time", "1", "2", "3", "4", "5")}
    	df.rg

    	df.rg_long <- melt(df.rg, id="time")  # convert to long format
    
    	# Plot
    	pRG <- ggplot(data=df.rg_long, aes(x=time, y=value, colour=variable)) + 
    		geom_line()
    
        # Set manual colors and label the axis
    	pRG + scale_colour_manual(values = c, labels = l) + 
    		xlab("Time (ps)") + ylab("Rg (nm)") 

    	         
   	# Save the plot as image in PNG & PDF formats with following attributes
    	ggsave(paste0(o,".png", sep=""),height=5,width=10,dpi=300) 
    	ggsave(paste0(o,".pdf", sep=""),height=5,width=10)
    	}  

# Plot RG
if (option == "rg"){
	plotRG(count, dataTable, outfilename, labels, colors)
}

######### RMSF backbone and sidechain #################
plotRMSF <- function(a,b,o,l,c){
	# define column of time
    	residue<-b[[1]][1]
    	# create a dataframe
    	df.rf <- data.frame(residue)

    	# iterate over the number of files set for plotting
    	for (i in 1:a){
        	#create a subset for the cols specifying RMSF
        	sub.rf <- b[[i]][2]
    
        	# bind the cols to the dataframe
        	df.rf <- cbind(df.rf, sub.rf)
        	i <- i+1
   	}	
    	# change all colnames
    	if (a ==1){ colnames(df.rf) <- c("residue", "1")}
    	if (a ==2){ colnames(df.rf) <- c("residue", "1", "2")}
    	if (a ==3){ colnames(df.rf) <- c("residue", "1", "2", "3")}
    	if (a ==4){ colnames(df.rf) <- c("residue", "1", "2", "3", "4")}
    	if (a ==5){ colnames(df.rf) <- c("residue", "1", "2", "3", "4", "5")}
    	df.rf    
    
    	df.rf_long <- melt(df.rf, id="residue")  # convert to long format
 
    	# Plot
    	pRf <- ggplot(data=df.rf_long, aes(x=residue,y=value,colour=variable)) +
	  	geom_line()
    
   	# Set manual colors and label the axis
   	pRf + scale_colour_manual(values = c, labels = l) + 
    		xlab("Residue") + ylab("RMSF (nm)") 

    
    	# Save the plot as image in PNG & PDF formats with following attributes
    	ggsave(paste0(o,".png", sep=""),height=5,width=10,dpi=300) 
    	ggsave(paste0(o,".pdf", sep=""),height=5,width=10)
    	
}  

# plot RMSF
if (option == "rf"){
	plotRMSF(count, dataTable, outfilename, labels, colors)
}


######### Plot  den-pres-pot-temp #################
plotSim <- function(a,b,o,l,s,c){
	# define column of time
    	time<-b[[1]][1]
    	# create a dataframe
    	df.sim <- data.frame(time)

    	# iterate over the number of files set for plotting
    	for (i in 1:a){
        	#create a subset for cols specifying den,pres,pot,temp
        	sub.sim <- b[[i]][2]
    
        	# bind the cols to the dataframe
        	df.sim <- cbind(df.sim, sub.sim)
        	i <- i+1
    	}	
    	# change all colnames
    	if (a ==1){ colnames(df.sim) <- c("time", "1")}
    	if (a ==2){ colnames(df.sim) <- c("time", "1", "2")}
   	if (a ==3){ colnames(df.sim) <- c("time", "1", "2", "3")}
    	if (a ==4){ colnames(df.sim) <- c("time", "1", "2", "3", "4")}
    	if (a ==5){ colnames(df.sim) <- c("time", "1", "2", "3", "4", "5")}
    	df.sim

    	df.sim_long <- melt(df.sim, id="time")  # convert to long format
    
    	# Plot
    	psim <- ggplot(data=df.sim_long, aes(x=time,y=value,colour=variable)) + 
    		geom_line()
    
    	# set manual colors and label the axis
    	if (s == "d"){
		psim + scale_colour_manual(values = c, labels = l) + 
    			xlab("Time (ps)") + ylab("Density (kg/m^3)") 
		}
    
    	if (s == "t"){
		psim + scale_colour_manual(values = c, labels = l) + 
    			xlab("Time (ps)") + ylab("Temperature (K)") 
		}

    	if (s == "p"){
		psim + scale_colour_manual(values = c, labels = l) + 
    			xlab("Time (ps)") + ylab("Pressure (bar)") 
		}

    	if (s == "e"){
		psim + scale_colour_manual(values = c, labels = l) + 
    			xlab("Time (ps)") + ylab("Potential Energy (kJ/mol)") 
		}   

    	# Save the plot as image in PNG & PDF formats with following attributes
    	ggsave(paste0(o,".png", sep=""),height=5,width=10,dpi=300) 
    	ggsave(paste0(o,".pdf", sep=""),height=5,width=10)
}  

# Plot density or temperature or potential energy or pressure
if (option == "d" | option == "t" | option == "p" | option == "e"){
	plotSim(count, dataTable, outfilename, labels, option, colors)
}

###### Plot secondary structure ##############################
plotSS <- function(a,b,o,l,c){
	# define column of time
    	time<-b[[1]][1]
    	# create dataframes for type of secondary structure
    	df.ss <- data.frame(time)
    	df.coil <- data.frame(time)
    	df.sheet <- data.frame(time)
    	df.helix <- data.frame(time)
    	df.turn <- data.frame(time)

    	# iterate over the number of files set for plotting
    	for (i in 1:a){
        	#create subsets for cols specifying various secondary structures
        	sub.ss <- b[[i]][2]
        	sub.coil <- b[[i]][3]
        	sub.sheet <- b[[i]][4]
        	sub.helix <- b[[i]][8]
        	sub.turn <- b[[i]][7]
		
		# name for grid view file
        	grid <- paste0("grid", i)
    
    		# bind cols to dataframes, and create two dataframes:one for
		# overall secondary structure comparison and second for grid
		# view of various secondary structure elements for each file
    		df.ss <- cbind(df.ss, sub.ss)
    		grid <- data.frame(time)
    		grid <- cbind(grid, sub.coil)
    		grid <- cbind(grid, sub.sheet)
    		grid <- cbind(grid, sub.helix)
    		grid <- cbind(grid, sub.turn)
    
   		# change col names for the grid
    		colnames(grid) <- c("time", "coil", "sheet", "helix", "turn")
    
    		# secify name of output file for the grid
    		file <- paste0("ss_file", i, ".pdf")
    		pdf(file,height=15,width=25)
    
   		# Plots for grid
    		cc<- ggplot(data=grid,aes(x=time,y=coil),
			environment= environment()) + 
			geom_line(color = "blue")+
			labs(title="Coil",x="[ps]",y="[no of residues]")
  
    		sh<- ggplot(data=grid,aes(x=time,y=sheet),
			environment= environment()) +
			geom_line(color = "orange")+ 
			labs(title="Sheet",x="[ps]",y="[no of residues]")

    		tu<- ggplot(data=grid,aes(x=time,y=turn),
			environment= environment()) +
        		geom_line(color = "green")+ 
			labs(title="Turn",x="[ps]",y="[no of residues]")

    		he<- ggplot(data=grid,aes(x=time,y=helix),
			environment= environment()) +
        		geom_line(color = "purple")+ 
			labs(title="Helix",x="[ps]",y="[no of residues]")

    		grid.arrange(cc, sh, tu, he, ncol=2)
    		i <- i+1
    	}
    	# change colnames of the dataframe for overall secondary str 
    	if (a ==1){ colnames(df.ss) <- c("time", "1")}
    	if (a ==2){ colnames(df.ss) <- c("time", "1", "2")}
    	if (a ==3){ colnames(df.ss) <- c("time", "1", "2", "3")}
    	if (a ==4){ colnames(df.ss) <- c("time", "1", "2", "3", "4")}
    	if (a ==5){ colnames(df.ss) <- c("time", "1", "2", "3", "4", "5")}
    	df.ss
    
    	df.ss_long <- melt(df.ss, id="time")  # convert to long format
    
  	# Plot
    	pSS <- ggplot(data=df.ss_long, aes(x=time, y=value, colour=variable)) + 
    		geom_line()
    
    	# set manual colors and label the axis
    	pSS + scale_colour_manual(values = c, labels = l) + 
    		xlab("Time (ps)") + ylab("Number of Residues") 
          
    	# Save the plot as image in PNG & PDF formats with following attributes
    	ggsave(paste0(o,".png", sep=""),height=5,width=10,dpi=300) 
    	ggsave(paste0(o,".pdf", sep=""),height=5,width=10)

}  

# ploy secondary structure
if (option == "s"){
	plotSS(count, dataTable, outfilename, labels, colors)
}
###############################################################################
###########################~~~~END~~~~~~#######################################
