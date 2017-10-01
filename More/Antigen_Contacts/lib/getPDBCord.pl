#!/usr/bin/perl -w

#
# coordExtract.pl
#
# parser that allows editing a pdb
# file to extract the coordinates
#
# Jorge Amigo Lechuga
#

#######################
# READ THE INPUT FILE #
#######################

# input file query
print "\nEnter the input file: ";
$inputFile = <STDIN>;
chomp $inputFile;

unless (open(INPUTFILE, $inputFile)) {
    print "Cannot read from '$inputFile'.\nProgram closing.\n";
    <STDIN>;
    exit;
}

# load the file into an array
chomp(@dataArray = <INPUTFILE>);

# close the file
close(INPUTFILE);

###############
# LET'S WORK! #
###############

# parse the input file saving only backbone atoms coordinates
# format: [string "ATOM"] [number] [atom] [aa] whateva [3 decimal numbers] whateva with two dots in between
for ($line = 0; $line < scalar @dataArray; $line++) {
    if ($dataArray[$line] =~ m/ATOM\s+\d+\s+(\w+)\s+\w{3}\s+.+\s+(\S+\.\S+)\s+(\S+\.\S+)\s+(\S+\.\S+)\s+.+\..+\..+/ig) {
        if ($1 eq "N" || $1 eq "CA" || $1 eq "C") {
            $parsedData{$line} = $2."\t".$3."\t".$4;
        }
    }
}

# create the output file name
$outputFile = "coordinates_".$inputFile;

# open the output file
open (OUTFILE, ">$outputFile");

# print the data lines
foreach $line (sort {$a <=> $b} keys %parsedData) {
    print OUTFILE $parsedData{$line}."\n";
}

# close the output file
close (OUTFILE);

# end message
print "The coordinates of '$inputFile' were saved into '$outputFile'.\n";

# end the program
exit;

