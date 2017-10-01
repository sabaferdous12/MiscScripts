use strict;
use warnings; 
use Data::Dumper;


print "\nEnter the input file: ";
#my $inputFile = <STDIN>;
my $inputFile = "1A14_1.pdb";

unless (open(INPUTFILE, $inputFile)) {
    print "Cannot read from '$inputFile'.\nProgram closing.\n";
   #<STDIN>;
    exit;
}

chomp (my @dataArray = <INPUTFILE>);

# close the file
close(INPUTFILE);
my %parsedData; 
my ($start, $end);
$start = 82;
my $chain = "N";



for (my $line = 0; $line < scalar @dataArray; $line++) {
    
    if (($dataArray[$line] =~ m/ATOM\s+\d+\s+(\w+)\s+\w{3}\s+$chain\s+$start\s+(\S+\.\S+)\s+(\S+\.\S+)\s+(\S+\.\S+)\s+.+\..+\..+/ig) ){   

	#print "Hello\n";
	

        if ($1 eq "CA") {
	    $parsedData{$line} = $2."\t".$3."\t".$4;
	    print Dumper $parsedData{$line}; 
	    
	}
	
    }
 #   $startRes++;

}




sub getRegionRange
{

    my ($regions) = @_;
    my (@splittedRegion, @rangeRegion, @regions);
    @regions = split (/,/, $regions);
    foreach my $region(@regions)
    {
        @splittedRegion = split (" ", $region);
        my $lastElement = pop @splittedRegion;
        push ( @rangeRegion, [$splittedRegion[0], $lastElement ] )
    }

    return @rangeRegion;

}

