package RbhBlastV1;

# Description: This module is used with the program rbhBlast.pl
# Author: Avneet Saini
# Date: 14 July 2014

use Exporter qw(import);
use LWP::Simple;
use File::Basename;
use List::Util qw(min max);
use List::MoreUtils qw(uniq);


our @EXPORT_OK = qw (readDirFiles getSeqFiles getOutputFilename getHitsFiles getQueryOrgName getBestFBlastRecFiles getForwardFsaFiles getFastaFiles 
getSpeciesListFile getOrthologues getRbhResult getQueryId getOrgNames 
getBestOrth getMaxScore doMSA doMSAclw doMSAfsa sortRecordsBySpecies helpMessage);
#******************************************************************************
#
# Description:      Reads the current directory and returns all files ending
#                   with file extension specified in '$fileExt'
# Input:            path of directory and file extension
# Output:           Array with all filenames with that extension
# Subroutine call:  readDirFiles($dir, $fileExt);

sub readDirFiles
{
my ($dir, $fileExt) = @_;
my @files;

opendir (my $dr, "$dir") or die "cannot open dircetory: $!\n";

while (my $file = readdir($dr)) {
next unless (-f "$dir/$file");                  #read only files
# read files with the extension 'fasta' that contain query protein sequences in
# fasta format
next unless ($file =~ m/$fileExt$/);           
		    push (@files, $file);
	}
closedir ($dr);
return @files;
}

#-----------------------------------------------------------------------------
sub getSeqFiles
{
    my ($filename, $dir)=@_;
    my $a = '.';

   #Read the INPUT File
    open (my $INPUT, "$filename")or die "File $filename cannot be opened\n"; 
    my $seqString = "";
    {
	local $/;
	$seqString = <$INPUT>;
    }
    close $INPUT;
    my @seqEntries = split(">", $seqString);
    
# Remove blank element from array
    @seqEntries = grep /\S/, @seqEntries;
   
# Remove leading and trailing white space from array elements
    my @newSeqArray = grep(s/\s*$//g, @seqEntries);
   
    foreach my $seq(@newSeqArray)
    {
	my @values = split (/\||\_/, $seq);
	my $proteinName = $values[2];
	my $protein = lc ($proteinName);
	# write each sequence in a seperate file in the directory rbh_results
	open (my $SEQFILE, "> $dir/$protein.fasta") or die "Error!";
	print {$SEQFILE} ">$seq";
    }
    
}
#----------------------------------------------------------------------------
sub getQueryOrgName
{
my ($filename, $dir)= @_;
my $org;
# open and read file

open (my $FASTAFILE, "< $dir/$filename") or die "Error $dir/$filename= $!";
    
my $string = "";
    {
    	local $/;
	$string = <$FASTAFILE>;
        }
   
        my @fileEntries = split (/\_|\s+/, $string);   
 
 $org =$fileEntries[1];

return $org;
}

#------------------------------------------------------------------------------
# Description:      Reads a filename and return various output filenames 
# Input:            a variable containing a filename
# Output:           Output filenames
# Subroutine call:  getOutputFilename($blastFilename);

sub getOutputFilename{
my ($filename, $suffix)=@_;

# Various extensions for the output filenames
my @exts = qw(hits forward fsa reverse orth orthfsa);
my @outFiles;

# get the basename of the input filename
my $basename = fileparse($filename, $suffix);
# define output filenames
foreach my $ext(@exts)
{
    my $outFilename = $basename.$ext;
    push (@outFiles, $outFilename);
}
return @outFiles;
}

#------------------------------------------------------------------------------
# Description:      Extracts only the blast alignment summaries from the
#                   blast output file
# Input:            blast output file ($blastFilename)
# Output:           blast alignment summaries file ($blastHitsFilename)
# Subroutine call:  getHitsFiles($blastFilename, $blastHitsFilename);


sub getHitsFiles{

my ($blastFilename, $blastHitsFilename)=@_;
# Read the blast output File
open (my $BLAST, $blastFilename) or die "File $blastFilename cannot be opened\n";
	
# Open a new file to write 
	open (my $HITS, '>', $blastHitsFilename);
	while (<$BLAST>)
	{
# Read everything that starts with '>' followed by anything and then ends with 
# either 'Gaps' or 'Positives' followed by anything n then ending with ')' 
		if (/^>/../Gaps|Positives.*\)$/) 
			{               
     			print $HITS "$_";
    			}
	}
}
#---------------------------------------------
sub sortRecordsBySpecies
{
    my ($dir, $blastFilename, $blastHitsFilename, $speciesFile)=@_;
   
# Read the blast File records
    open (my $BLAST, $blastFilename) or die "File $blastFilename cannot be opened\n";
# Read species names
    open (my $SPFILE, "< $dir/$speciesFile") or die "File $dir/$speciesFile cannot be opened\n";
   # Open a new temporary file to write sorted records
	open (my $TEMP, "> $dir/blasthits.tmp");
 # Open a new file to write the record with max score for each species
    open (my $SORT, "> $blastHitsFilename");

    
    while (<$BLAST>)
	{
# Read everything that starts with '>' followed by anything and then ends with 
# either 'Gaps' or 'Positives' followed by anything n then ending with ')' 
	   	if (/^>/../Gaps|Positives.*\)$/) 
		{      
		    print $TEMP "$_";
		}
		
	}
    close $TEMP;
    close $BLAST;

# Read the temp file
    open (my $TEMPFILE, "< $dir/blasthits.tmp");
    my $hitString = "";
    {
    	local $/;
	$hitString = <$TEMPFILE>;
    }
    close $TEMPFILE;


    my @hitEntries = split(">", $hitString);
   # Remove blank element from array
    @hitEntries = grep { $_ ne '' } @hitEntries;

    open (my $TP1, "> $dir/blasthits1.tmp");
foreach my $hit(@hitEntries)
	    {
		$hit =~ s/\n|\s+/ /g;
		$hit=~ s/\s+/ /g;
	
		print {$TP1} "$hit", "\n";
	    }
    close $TP1;

    open (my $READTP1, "< $dir/blasthits1.tmp");
    my @recordsArray;
    while (my $entry= <$READTP1>)
    {
	push (@recordsArray,$entry) 
    }
   
    while (my $orgName = <$SPFILE>)
	{
	    chomp $orgName; 
	    
	   foreach my $recordEntry(@recordsArray)
	   {   
		
		if ($recordEntry =~ m/$orgName/)
		{	
		    print {$SORT} "$recordEntry", "\n";
		    last;
		}
	
		       
	    }
	}
    unlink glob "$dir/blasthits.tmp";	
    unlink glob "$dir/blasthits1.tmp";	
   
}

#----------------------------------------------------------------------------
# Description:      Get an array with all query species
# Input:            variable containing filename 
# Output:           Array with all species names
# Subroutine call:  getSpeciesListFile($speciesFilename);

sub getSpeciesListFile
{

    my ($filename, $species, $dir) = @_;
    
    my @sets = split(/\./, $filename);
    my $outfile = "$sets[0].sp";
    open(my $OUT_FILE, "> $dir/$outfile")
	or die "Error $outfile = $!\n";
    
# Read file containing species names
    open(my $INPUT_FILE, $filename) or die "Couldn't open $filename!";
    my @speciesSet;
    while (my $line = <$INPUT_FILE>)
    {
	my @speciesnames;
	if ($line =~ m/^(\d+)/)
	{
	    # if the file is generated by findspecis.pl then remove leading
	    # and trailing characheters else if its plain text file then make array	    
	    my @sections = split (/^\d+\s|\s+species|subspecies/, $line);
	    if ($sections[1] !~ m/^$species$/)	
	    {
		print $OUT_FILE "$sections[1]\n"; 
	#	push (@speciesSet, $sections[1]);       
	    }
	}

	elsif ($line =~ m/^(\D+)/)
	{
	  #  push (@speciesSet, $line);
	    print $OUT_FILE "$line";
	}
    }
   # return @speciesSet;
    
}

#------------------------------------------------------------------------------
# Description:      Gets fasta sequences for GIs
# Input:            File containing alignment summaries($forwardResultFilename)
# Output:           File containing FASTA sequences ($ForwardFsaFilename)
# Subroutine call:  getForwardFsaFiles($forwardResultFilename,
#                   $ForwardFsaFilename);

sub getForwardFsaFiles{
my ($resultFilename, $ForwardFsaFile, $dir)=@_;
my @fgiList;

# If file is empty then skip it
if (-z "$resultFilename")
{
    last;
}
else
{
# Read contents of input file
    open (my $FBLAST, "< $resultFilename") or die "File $resultFilename 
cannot be opened\n";
    while (<$FBLAST>)
    {
	if ($_ =~ m/\|/) # split contents at '|' to get the GIs for each record
	{
	    my (@fields) = split (/\|/, $_);
	    push (@fgiList,  $fields[1]);
	}
    }

#check if the query proteins' GI is in the list and remove it
my @words = split (/\./, $resultFilename);
my $queryWord = $words[0];
my $queryId= getQueryId($queryWord, $dir);

   
my @newFgiList;
foreach my $gi(@fgiList)
{
    if ($gi !~ $queryId)
    {
	push (@newFgiList, $gi);
    }
}

getFastaFiles ($dir, $ForwardFsaFile, @newFgiList);
}
}
#-------------------------------------------------------------------------------
sub getFastaFiles{
my ($dir, $filename, @giList)=@_;

# string of query GIs seperated by comma
my $giString = join (',', @giList);

# To get the FASTA sequences define the link
my $prefix = "http://www.ebi.ac.uk/Tools/dbfetch/dbfetch?db=uniprot&id=";
my $suffix = "&format=fasta&style=raw";
my $url = $prefix.$giString.$suffix;

# Open output file to write fasta sequences
open (my $FILE, "> $filename");

# call the URL to get the fasta sequences and write to the output file
my $fsa = get ("$url");
if ($fsa) 
			{        
       			 print {$FILE} "$fsa\n";
      			}
    
		else 	
			{
        		warn "cannot retrieve value \n";
   			}


}
#-------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Description:      gets a list of the best hits from 'mouse' for all the query
#                   GIs in the input file 
# Input:            reverse blast output file $reverseBlastOutFilename
# Output:           Array of orthologues
# Subroutine call:  getOrthologues($reverseBlastOutFilename);

sub getOrthologues {
my ($file, $dirPath, $queryOrgName)=@_;
my (@orth, @query, @bestOrth);

# Read input file contents
open (my $RBLAST, "< $dirPath/$file");
while (<$RBLAST>)
{
	

	if ($_ =~ m/$queryOrgName/)        
		{
		push (@orth, $_);  # array of all hits for mouse for each query
		my @fields = split (/\|/,  $_);
		push (@query, $fields[1]);   # gets GIs for the queries
		}
}

my @unique_queries = uniq @query;  # removes duplication of the query GIs

my @list;
# for each query GI an array of all the hits for mouse is built and then the
# ones with the highest score are selected
foreach my $id(@unique_queries)
{
my @orthSet;
	foreach my $entry(@orth)
	{
	if ($entry =~ m/$id/)
		{
		push (@orthSet, "$entry\n");
		}
	}

#gets all the hits with maximum score for one query GI
my @bestHit= getBestOrth(@orthSet);

# array containing all hits with maximum score for all query GIs in the file      
push (@list, @bestHit);    

}
return @list;

}

#------------------------------------------------------------------------------
# Description:      Gets the records with maximum score
# Input:            Array of blast records
# Output:           Array with only records corresponding to the maximum score
# Subroutine call:  getBestOrth(@orthSet);


sub getBestOrth{
my (@orthSet)= @_;
my @bestOrthologues;

# gets Maximum score from an array of records
my $maxScore= getMaxScore(@orthSet);

foreach my $line(@orthSet)
	{
	if ($line =~ m/$maxScore/)     # checks if record matches maximum score
		{
		push (@bestOrthologues, $line);
		} 
	}
return @bestOrthologues;

}

#------------------------------------------------------------------------------
# Description:      Reads the current directory and returns all files ending
#                   with file extension specified in '$fileExt'
# Input:            Array of best hits from  mouse for different queries
# Output:           Two Files; i) $orthHitsFilename: that contains a list of
#                   the query GIs, species in which orthologs were found and
#                   also the ortholog Gi. ii) complete.$orthHitsFilename:
#                   contains a list of the query GIs, all species that were
#                   searched followed by the ortholog Gi were ever ortholog was#                   found.
#                   The output file format is
#                   the Query GI#Organism Name___Ortholog GI
# Subroutine call:  getRbhResult($orthHitsFilename, @orthList);

sub getRbhResult
{
    my ($orthHitsFilename, $orthProteinsSeqFilename, $dirPath, $dir,
	$orthListRef)= @_;
    my @orthList = @$orthListRef;

    my $orthProteinsSeqFilename_path = $dirPath."/$orthProteinsSeqFilename";
    my $detailResultFilename = "complete_".$orthHitsFilename;

# get list of organsims in the blast output format
    my @organismlist = getOrgNames(@orthList);
    
    my @organismNames = uniq @organismlist;  # removes duplication
    my @orthGiList;
    
    my @words= split (/\./, $orthHitsFilename);
    my $queryWord = $words[0];              # gets the name of the query protein
    my $queryId= getQueryId($queryWord, $dir);# gets the GI of the query protein

    push (@orthGiList, $queryId);

#Open two files to write output
    open (my $OUTPUT, "> $dirPath/$orthHitsFilename");
    open (my $DETAILOUTPUT, "> $dirPath/$detailResultFilename");

# print headers in the output files
    print $OUTPUT "QUERY#ORGANISM___ORTHOLOG\n\n";
    print $DETAILOUTPUT "Query#ORGANISM___ORTHOLOG\n\n";
    
# for every orgaism, the input array is checked for the the presence of the
# query GI and the output written in new files
    foreach my $organism(@organismNames)
    {
	foreach my $hit (@orthList)
	{
	    
	    chomp ($organism, $queryId);
	    if  (($hit =~ /$organism/) and ($hit =~ m/$queryId/))
	    {
        	my @word = split (/\|/, $hit);
		print $OUTPUT "$queryId#$organism ___$word[1]\n";
		print $DETAILOUTPUT "$queryId#$organism ___$word[1]\n";
		push (@orthGiList, $word[1]);
	    }
	    elsif(($hit =~ /$organism/) and ($hit !~ m/$queryId/))
	    {
		print $DETAILOUTPUT "$queryId#$organism ___NULL\n";
	    }
	}
	
    }
    
    my $giCount = scalar @orthGiList;
    if ($giCount > 1)
    {   
	getFastaFiles($dirPath, $orthProteinsSeqFilename_path, @orthGiList);
    }
    else
    {
	print "No orthologs for $orthHitsFilename\n";
    }
}

#------------------------------------------------------------------------------
# Description:      Gets the GI from a FASTA sequence file 
# Input:            variable containing name of protein that matches the
#                   basename of the fasta sequnece filename
# Output:           GI
# Subroutine call:  getQueryId($queryWord);
sub getQueryId{
my ($word, $dir)= @_;

my @words = split ('\/', $queryWord);
my $queryWord = $words[1];

# Read current directory for files with extension '.fasta'

my $ext='.fasta';
my @inputQueryFilenames= readDirFiles($dir, $ext);
my $queryId;

foreach my $filename(@inputQueryFilenames)
{
	chomp $filename;
	chomp $queryWord;

	my $function = index ($filename, $queryWord);
# checks if the word passed to the subroutine is the basename of the filename  
	if ($function != -1)  
		{
# Open a file to read
		open (my $QUERYFILE, "< $dir/$filename") or die "ERROR";
		while (my $string = <$QUERYFILE>)
			{
			my @queries= split (/\|/, $string);
			$queryId= $queries[1];
			last;
			}
		}
}
return $queryId;
}
#------------------------------------------------------------------------------
# Description:      Gets organism names as given in the blast tabular output
# Input:            Array of blast records
# Output:           Array of organsim names
# Subroutine call:  getOrgNames(@orthList);

sub getOrgNames{
my (@orth)=@_;
my @Names;
foreach my $hit(@orth)
	{
	my @list = split (/\_|\s+/, $hit);
	push (@Names, "$list[1]\n");
	}

return @Names;
}

#------------------------------------------------------------------------------
# Description:      Reads array of blast records and finds maximum score
# Input:            Array of blast records
# Output:           Maximum Score
# Subroutine call:  getMaxScore(@orthSet);

sub getMaxScore{
my (@array)=@_;
my @score;

foreach my $hit(@array)
	{	
	my @columns = split (' ', $hit);   #split at space
	push (@score, "$columns[11]\n");   # 12th column contains score
	}
my $maxScore = max (@score);    # calculates maximum from an array of numbers
return $maxScore;
}

#------------------------------------------------------------------------------

sub doMSA
{
    my ($muscle, $dir, $subDir)=@_;
    
    my $subDirPath = $dir."/$subDir";   
    my $fileSuffix = "orthfsa";
    my @orthfsaFiles = readDirFiles($subDirPath, $fileSuffix);
	   my $count = scalar @orthfsaFiles;
    if ($count == 0)
    {
	print "No Orthologs for $subDir!\n";
    }
    else
    { 
    foreach my $orthfsaFile(@orthfsaFiles)
    {
	my @xyz = split (/\./, $orthfsaFile);
	my $fileBasename = $xyz[0];
	my $msa_clwFilename= $fileBasename.".clwstrict";
	my $msa_fsaFilename= $fileBasename.".clw";
      
      
	`$muscle -in $subDirPath/$orthfsaFile -clwstrict -out $subDirPath/$msa_clwFilename`;
      `$muscle -in $subDirPath/$orthfsaFile -out $subDirPath/$msa_fsaFilename`
    }
    }
}

#-----------------------------------
sub  doMSAclw
{
    my ($muscle, $dir, $subDir)= @_;

    my $subDirPath = $dir."/$subDir";
    my $fileSuffix = "orthfsa";
    my @orthfsaFiles = readDirFiles($subDirPath, $fileSuffix);
    my $count = scalar @orthfsaFiles;
    if ($count == 0)
    {
	print "No Orthologs for $subDir!\n";
    }
    else
    {
    
    foreach my $orthfsaFile(@orthfsaFiles)
    {
	my @xyz = split (/\./, $orthfsaFile);
	my $fileBasename = $xyz[0];
	my $msa_clwFilename= $fileBasename.".clwstrict";
     
	`$muscle -in $subDirPath/$orthfsaFile -clwstrict -out $subDirPath/$msa_clwFilename`;
    }
    }
}

#-----------------------------------------

sub doMSAfsa
{
    my ($muscle, $dir, $subDir)=@_;

    my $subDirPath = $dir."/$subDir";
    my $fileSuffix = "orthfsa";
    my @orthfsaFiles = readDirFiles($subDirPath, $fileSuffix);
	  my $count = scalar @orthfsaFiles;
    if ($count == 0)
    {
	print "No Orthologs for $subDir!\n";
    }
    else
    {  
    foreach my $orthfsaFile(@orthfsaFiles)
    {
	my @xyz = split (/\./, $orthfsaFile);
	my $fileBasename = $xyz[0];
	my $msa_fsaFilename= $fileBasename.".clw";
	      
	`$muscle -in $subDirPath/$orthfsaFile -out $subDirPath/$msa_fsaFilename`
    }
    }
}

#-----------------------------
sub helpMessage
{
print <<EOF;

Synopsis: 

Keep the script in a directory that contains the input file (containing one or \
more fasta sequences) and the subject-species file.
The script will make a new sub-directory to write all the output files (rbh_results).\
If this directory already exists then it will ask for a directory name and write all \
output files to this new sub-directory. 

It will then:

i) Reads a file containing one or more fasta sequences from the input file \
specified by the '-i' flag and perform a blast run. 

ii) Next, it reads the blast output files, selects the best hit for each query \
species specified by the flag '-s' against a set of subject-species listed in a \
file specified by the flag '-f'.The input species file  may be a plain text file \
in which each line corresponds to the scientific name of one species or it may \
be a file that is created by the findSpeciesInGenusTest.pl script \
(http://nebc.nerc.ac.uk/tools/code-corner/scripts/biological-database-searching \
-and-results-processing)

iii) then performs a reverse blast to check orthologs. 
The flag '-o' is only used when the first blast run has already been done and \
the script should start only from the reverse blast onwards. For this option, \
you still need the inputfile with fasta sequences and also blast output files \
for each fasta sequence saved as *.blast in the current directory. the * in \
the filename corresponds to the protein name as given in the fasta header. For \
example in the header line ">sp|P19246|NFH_MOUSE Neurofilament heavy polypeptide \
OS=Mus musculus GN=Nefh PE=1 SV=3" the * is nfh(in lower case). So the blast \
output filename should be nfl.blast.

iv) It then performs Multiple Sequence Alignment for the
orthologs thus identified, if the flag '-m' is specifeid. This
flag can have the option 'fasta, clw, both' for the type of
output file format.

Output:        It returns files in a sub-directory (rbh_results or user
               specified name ) at the end. The various file in the directory
               are as described:
               *.fasta:             fasta sequence file for each entry in the
                                    inputfile
               *.blast:             Blast results file
               *.forward:           contains the best match for each subject
                                    species
               *.fsa :              contains sequence for each hit in the 
                                    *.forward file
               *.reverse:           reverse blast alignment table results for 
                                    the sequences in *.fsa file
               *.orth:              contains summary about orthologs
               complete_*.orth:     details for ortholog searches
               *.orthfsa:           fasta sequences of the orthologs. First
                                    entry is the query sequence
               *.clw:               Multiple Sequence Alignment in fasta format
               *.clwstrict:         Multiple Sequence Alignment in clustal
                                    format
********************************************************************************
USAGE:

 ./rbhBlast-v1.pl -i <input sequence filename> -s <Query species name> -f \
<Subject-species Filename> -o <output type> -m <MSA type>

Example:

./rbhBlast-v1.pl -i protein.txt -s Mus\_musculus -f subject_species.txt \
-o reverse -m both

   
*****************************************************************************
EOF
}

















1
