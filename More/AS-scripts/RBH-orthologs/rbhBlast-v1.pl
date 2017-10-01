#!/usr/bin/perl 
#
#******************************************************************************
#                      
# Program:      rbhBlast-v1
# File:         rbhBlast-v1.pl
# USAGE:        ./rbhBlast-v1.pl -i <input sequence filename> 
#                                -s <Query species name>
#                                -f <Subject-species Filename>
#                                -o <output type>
#                                -m <MSA type>
#               eg. ./rbhBlast-v1.pl -i protein.txt -s Mus\_musculus 
#                   -f subject_species.txt -o reverse -m both
#
#               ./rbhBlast-v1.pl -h   (for help)
# Subroutines:  in module, RbhBlastV1.pm
# 
# Date:         16 Jan 2015
# Function:     Find orthologs for a query protein(query species) for a list
#               of user defined species(Subject species) 
#               
# 
# Author:       Avneet Saini
# Address:      Institute of Structural and Molecular Biology
#               Division of Biosciences
#               University College
#               Gower Street
#               London
#               WC1E 6BT
# Email:        avneetsaini@gmail.com
#******************************************************************************
# Description:  This script is used to find orthologs for query species.
#
# Synopsis:     Keep the script in a directory that contains the input file and
#               the species file. The script will make a new sub-directory to 
#               write all the output files(rbh_results).If this directory 
#               already exists then it will ask for a directory name and write 
#               all output files to this new sub-directory. 
#               It will then i) Read a file containing one or
#               more fasta sequences from the input file specified by the '-i'
#               flag  and perform a blast run. ii) Next, it reads the blast 
#               output files, selects the best hit for each query species
#               specified by the flag '-s' against a set of subject-species
#               listed in a file specified by the flag '-f'.The input species
#               file  may be a plain text file in which each line corresponds
#               to the scientific name of one species or it may be a file that
#               is created by the findSpeciesInGenusTest.pl
#               script (taken from the website: 
#               http://nebc.nerc.ac.uk/tools/code-corner/scripts/
#               biological-database-searching-and-results-processing)
#               iii) then performs a reverse blast to check orthologs.
# 
#               The flag '-o' is only used when the first blast run has already
#               been done and the script should start only from the reverse
#               blast onwards. For this option, you still need the inputfile
#               with fasta sequences and also blast output files for each fasta
#               sequence saved as *.blast in the current directory. the * in
#               the filename corresponds to the protein name as given in the
#               fasta header. For example in the header line
#               ">sp|P19246|NFH_MOUSE Neurofilament heavy polypeptide OS=Mus
#               musculus GN=Nefh PE=1 SV=3" the * is nfh(in lower case). So the
#               blast output filename should be nfl.blast.
#
#               iv) It then performs Multiple Sequence Alignment for the
#               orthologs thus identified, if the flag '-m' is specifeid. This
#               flag can have the option 'fasta, clw, both' for the type of
#               output file format.
#  Output:      It returns files in a sub-directory (rbh_results or user
#               specified name ) at the end. The various file in the directory
#               are as described:
#               *.fasta:             fasta sequence file for each entry in the
#                                    inputfile
#               *.blast:             Blast results file
#               *.forward:           contains the best match for each subject
#                                    species
#               *.fsa :              contains sequence for each hit in the 
#                                    *.forward file
#               *.reverse:           reverse blast alignment table results for 
#                                    the sequences in *.fsa file
#               *.orth:              contains summary about orthologs
#               complete_*.orth:     details for ortholog searches
#               *.orthfsa:           fasta sequences of the orthologs. First
#                                    entry is the query sequence
#               *.clw:               Multiple SEquence Alignment in fasta format
#               *.clwstrict:         Multiple SEquence Alignment in clustal
#                                    format
#               
#******************************************************************************

use strict;
use warnings;
#use IO::CaptureOutput qw(capture qxx qxy);
use Carp;
use Getopt::Std;
use ASrbhVars;
use RbhBlastV1 qw (readDirFiles getSeqFiles getOutputFilename getHitsFiles getQueryOrgName getBestFBlastRecFiles getForwardFsaFiles getFastaFiles getSpeciesListFile getOrthologues getRbhResult getQueryId getOrgNames getBestOrth getMaxScore doMSA doMSAclw doMSAfsa sortRecordsBySpecies helpMessage);

################################################################################
# Declare command line flags
my %options=();

# option s for the query species, f for tje species filename, o for the output fiunction
# and m for multiple sequence alignment
getopts("hi:s:f:o:m:", \%options);

# test for the existence of the options on the command line.
# in a normal program you'd do more than just print these.
#print "-s $options{s}\n" if defined $options{s};
#print "-f $options{f}\n" if defined $options{f};
#print "-o $options{o}\n" if defined $options{o};
#print "-m $options{m}\n" if defined $options{m};

#*******************************************************************************
# Define Inputs

if (defined $options{h})
{
   helpMessage;
    exit;
}

# Blast
my $blast = $ASrbhVars::blast;
# Uniprot database
my $uniprot = $ASrbhVars::uniprot;
# Muscle
my $muscle = $ASrbhVars::muscle;
# File containing list of species
my $speciesFilename = $options{f};
# Query species name
my $speciesName = $options{s};
# Input File with one or more fasta sequences
my $inputSeqFilename = $options{i};

#*******************************************************************************

# Current directory that contains the script and inputfiles for
# i) sequences & ii) species
my $currentDir='.';
# New directory that will contain all the orthologs results
my $dir;

# Check if directory already exists
my $ckDr = "rbh_results";
if (! -d $ckDr)
{
    mkdir $ckDr;
    $dir = $ckDr;
}
else
{
    print "Directory $ckDr exists!\n Type a name for Results Directory:";
    my $mkDr = <STDIN>;
    chomp $mkDr;
    if (! -d $mkDr)
    {
	mkdir $mkDr;
	$dir = $mkDr;
    }
    else
    {
	print "Error directory $mkDr already exists!\n";
	exit;
    }

}

my @abc = split (/\./, $speciesFilename);
my $speciesFile= $abc[0].".sp";

if (defined $inputSeqFilename)
{
    getSeqFiles($inputSeqFilename, $dir);
}
else
{
    print "Error Input Sequence file not specified!\n";
    exit;
}

# Read the fasta files 
my $fileExt1= '.fasta';
my @fastaFiles=readDirFiles($dir, $fileExt1);
@fastaFiles=sort@fastaFiles;
 
# Read a file containing information about the species names and get the names
# in an array. The file may be a plain txt file with scientific names of the 
# species one in each line or it may be a file generated by the findSpecies.pl
# script
for ($speciesFilename)
{
getSpeciesListFile($speciesFilename, $speciesName, $dir);
}

# If option 'o' is speceified than only reverse blast is to be performed 
if (defined $options{o})
{

    # Read blast ouput files from the current directory
    my $blastExt= '.blast';
    my @blastFiles=readDirFiles($currentDir, $blastExt);

    if (scalar @blastFiles == 0)
    {
	print "No blast output file found!\n";
	exit;
    }
    else
    {
	print "Starting reverse blast\n";
    }

    foreach my $blastFile(@blastFiles)
    {
	my @fieldsFile = split(/\./, $blastFile);
	my $outputName = $fieldsFile[0];

	my $fastaFilename = "$outputName.fasta";
 
        # Get the Query Organsim name in the short form name as given
        # in blast table output file
	my $queryOrgName = getQueryOrgName($fastaFilename, $dir);

	# New Directory for writing output files 
	my $proteinDirName = $dir."/$outputName";
	mkdir $proteinDirName;
	my $dirPath = $proteinDirName;

	# define names for files that will be written
	my $suffix = "blast";
	my @outputFilenames = getOutputFilename($blastFile, $suffix);
	my ($blastHitsFilename, $forwardResultFilename, $forwardFsaFilename,
	    $reverseBlastOutFilename, $orthHitsFilename,
	    $orthProteinsSeqFilename) = @outputFilenames;


	# specify name of blast output file
	my $blastOutputFilename= $currentDir."/$blastFile";   


	# Specify path of files to  write
	    my $forwardHitsFile=$dirPath."/$forwardResultFilename";
	  my $forwardFsaFile=$dirPath."/$forwardFsaFilename";

	# Read blast output file and write the hit with max score for each 
	# species in a new file
	sortRecordsBySpecies($dir, $blastOutputFilename, $forwardHitsFile, $speciesFile);
	
	# For each hit in the file; write the respective fasta sequences in a new file 
	getForwardFsaFiles($forwardHitsFile, $forwardFsaFile, $dir);

	# perform blast (reverse blast)
	system("$blast -F T -p blastp -a 2 -d $uniprot -i $dirPath/$forwardFsaFilename -m8 -o $dirPath/$reverseBlastOutFilename");

	# Read the reverse blast output file and builts an array with best hits
	# corresponding to the query species 
	my @orthList= 
	    getOrthologues($reverseBlastOutFilename, $dirPath, $queryOrgName);
	
	# Tests if the best hits from each species (after the first blast run) are 
	# the orthologs. Output is written to three new files i) <query>.orth 
	# ii) complete_<query>.orth (detail record for every entry,and iii)<query>.orthfsa 
	my @rbhOrthList =
	    getRbhResult($orthHitsFilename, $orthProteinsSeqFilename, $dirPath,
			 $dir, \@orthList);
	
    }


}

else
{

    print "Beginning the process\n";    
    # Perform blast search for each input file
    foreach my $inputFilename(@fastaFiles)
    {  
	# get the name of file without extension
	my @fieldsFile = split(/\./, $inputFilename);
	my $outputName = $fieldsFile[0];
    
	# Get the Query Organsim name as given in blast table output file
	my $queryOrgName = getQueryOrgName($inputFilename, $dir);

	# New Directory for writing output files 
	my $proteinDirName = $dir."/$outputName";
	mkdir $proteinDirName;
	my $dirPath = $proteinDirName;

	# specify name of blast input file
	     my $blastInput = $dir."/$inputFilename";
	# specify name of blast output file
	my $blastOutputFilename= $dir."/$outputName.blast";     
	print "Begin Forward Blast $inputFilename\n";
	# perform blast search using uniprot database, E value cut off= 0.005 
	system("$blast -F T -p blastp -a 2 -d $uniprot -e 0.005 -v 8000 -b 8000 -i $blastInput -o $blastOutputFilename");

	# define names for files that will be written
	my $suffix = "fasta";
	my @outputFilenames = getOutputFilename($inputFilename, $suffix);
	my ($blastHitsFilename, $forwardResultFilename, $forwardFsaFilename,
	    $reverseBlastOutFilename, $orthHitsFilename, $orthProteinsSeqFilename)
	    = @outputFilenames;

	# Specify path of files to  write
	    my $forwardHitsFile=$dirPath."/$forwardResultFilename";
	  my $forwardFsaFile=$dirPath."/$forwardFsaFilename";
	print "Sorting  Forward Blast Records...\n";
	# Read blast output file and write the hit with max score for each 
	# species in a new file
	sortRecordsBySpecies($dir, $blastOutputFilename, $forwardHitsFile, $speciesFile);
	
	# For each hit in the file; write the respective fasta sequences in a new file 
	getForwardFsaFiles($forwardHitsFile, $forwardFsaFile, $dir);
	
	print "Begin Reverse Blast for $inputFilename!\n";
	# perform blast (reverse blast)
	system("$blast -F T -p blastp -a 2 -d $uniprot -i $dirPath/$forwardFsaFilename -m8 -o $dirPath/$reverseBlastOutFilename");

	# Read the reverse blast output file and builts an array with best hits
	# corresponding to the query species 
	my @orthList= 
	    getOrthologues($reverseBlastOutFilename, $dirPath, $queryOrgName);
	
	# Tests if the best hits from each species (after the first blast run) are 
	# the orthologs. Output is written to three new files i) <query>.orth 
	# ii) complete_<query>.orth (detail record for every entry,and iii)<query>.orthfsa 
	my @rbhOrthList =
	    getRbhResult($orthHitsFilename, $orthProteinsSeqFilename, $dirPath,
			 $dir, \@orthList);
	print "RBH process complete for $inputFilename\n\n!!!";
    }

}

#*******************************************************************************
# Multiple sequence alignment of orthologs

if (defined $options{m})
{
    print "Begin MSA\n";
    # get all list of all sub-directories in the species directory
    opendir (my $dh, $dir);
    my @dirs = grep {-d "$dir/$_" && ! /^\.{1,2}$/} readdir($dh);
   
    foreach my $subDir(@dirs)
    {

	if ($options{m}=~ m/both/)
	{
	    doMSA($muscle, $dir, $subDir);

	}
	elsif ($options{m}=~ m/clw/)
	{
	    doMSAclw($muscle, $dir, $subDir);
	}
	elsif ($options{m}=~ m/fsa/)
	{
	    doMSAfsa($muscle, $dir, $subDir);
	}
    }
}


print "Process Ends!!";
