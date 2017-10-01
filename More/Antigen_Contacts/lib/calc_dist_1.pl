use strict;
#use warnings;

# This script finds the distance of each CA on straight line (drawn
# from first to last point )

use Data::Dumper;
use SFPerlVars;
#use Math::Vector::Real;
use Math::Vec qw(NewVec);

chdir "/acrm/data/people/saba/data/dataNew/NR_Complex_Martin/";
my $infile = "epitope_sequence-G3-CR3";
open (my $EPITOPE, '<', "./stats/$infile") or die
    "Can not open file";

my @antigen_DNA;
my @antigen_protein;
my %parsedData;
my @distancesCalphaToLine;
my @AlldistToLine;
my @distToLine;
my $antigen_chain_label;
my %distances;
while (<$EPITOPE>)
{
 
    my ($pdb_file, $regions, $fragments) = split (':', $_);

    if ($_ =~ /^Antibody/) # ignoring first line                              
    {
        next;
    }
    else
    {
        open (my $COMPLEX, $pdb_file) or die
            "Can not open $pdb_file"; # open PDB file                        

	chomp (my @dataArray = <$COMPLEX>);


	my ($antigen_chain_label, $antigen_chain_type) =
	    getChainLabelAndType($pdb_file);

        if ( ($antigen_chain_type eq 'DNA') or 
	     ($antigen_chain_type eq 'RNA') )
# To keep track of DNa antigens     
        {
            push (@antigen_DNA, $pdb_file);
            next;
        }

	else
	{
	    push (@antigen_protein, $pdb_file);
	    
	    my @regRange = getRegionRange($regions);
	    # To obtain the range on each epitopic region i.e, start and end

	    # The array of arrays containing distances to straight line for
            # all epitopic regions in one antibody 
	    my @distancesCalphaToLine = getDistanceToStraightLine 
		(\@regRange, \@dataArray, $antigen_chain_label);

	    # Hash of arrays to store regions corresponding to an antibody
	    $distances{$pdb_file} = [@distancesCalphaToLine];
	    
	}	    
	
    }
    #last;
   
}
################### Main Endssss ###############

#print Dumper (\%distances);
open (my $OUT, ">myDistances.txt") or
    die ("Can not open file for writing\n");

foreach my $key (keys %distances)
{

print $OUT "$key: ",  Dumper ($distances{$key});


}


########### Methods ###############

sub getDistanceToStraightLine
{
    my ($regRangeRef, $dataArrayRef, $antigen_chain_label) = @_;
    my $coords;
    my @coordsCA = ();
    my ($P, $P0, $P1);

    my @AlldistToLine = ();

    foreach my $record (@{$regRangeRef})
    {
#	my @AlldistToLine = ();
	my ($startRes, $endRes) = ($record->[0], $record->[1]);
	@coordsCA = getCoordsForRegion
	    ($dataArrayRef, $antigen_chain_label, $startRes, $endRes);
	my ($startP, $endP) = getRegionsEndCoords(@coordsCA);
        # CA coordinates of region start and end

	my ($P0_x, $P0_y, $P0_z) = getXYZ($startP);
	my ($P1_x, $P1_y, $P1_z) = getXYZ($endP);

	# Making 3D vectors of points
	$P0 = NewVec($P0_x, $P0_y, $P0_z);
        $P1 = NewVec($P1_x, $P1_y, $P1_z);
	my @distToLine = calculateDistanceRegion ($P0, $P1, @coordsCA);

	push (@AlldistToLine, [@distToLine] );
    }
    
    return @AlldistToLine;
   

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

    return @rangeRegion; # Array of anonymous arrays i.e containing addresses 
                         # of arrays

}


sub getChainLabelAndType
{
    my ($pdb_file) = @_;
    my $chaintype = $SFPerlVars::chaintype;
    # To obtain the antigen chain label and chain type (N Protein)         
    
    my ($antigen_chain_label, $antigen_chain_type) =
	split(" ", `$chaintype $pdb_file | head -1`);

    return $antigen_chain_label, $antigen_chain_type;

}


sub getCoordsForRegion
{
    my ($dataArrayRef, $antigen_chain_label, $startRes, $endRes) = @_;
    my (%parsedData, @coords, $coords);

    for (my $line = 0; $line < scalar @{$dataArrayRef}; $line++)
    {
	if($startRes<=$endRes)
	{     
	    
	    if (($$dataArrayRef[$line] =~ m/ATOM\s+\d+\s+(\w+)\s+\w{3}\s+$antigen_chain_label\s+$startRes\s+(\S+\.\S+)\s+(\S+\.\S+)\s+(\S+\.\S+)\s+.+\..+\..+/ig) )
	    {
		if ($1 eq "CA")
		{
		    $parsedData{$line} = $2."\t".$3."\t".$4;
		    $coords = $parsedData{$line};
		    push(@coords, $coords);
		    $startRes++;	    
		}
		
	    }
	}
	
    }
    return @coords;
}

sub getRegionsEndCoords
{
    my (@coordsCA) = @_;
    my ($startP, $endP);
    $startP= $coordsCA[0];
    $endP = pop @coordsCA;
    return $startP, $endP;

}

sub getXYZ
{
    my ($startP) = @_;
    my ($x, $y, $z) = split (" ", $startP);
    return $x, $y, $z;

}

sub calculateDistanceRegion
{

    my ($P0, $P1, @coordsCA) = @_;
    my ($P, $VL, $W, @distToLine, $dist, $product, $magnitudeVLxW, $magnitudeVL);
## This for loop reads all CAs excluding start and end 
## Starts from 2nd element and ends at second last element.    
    for (my $i = 1; $i<= (scalar @coordsCA)-2 ; $i++)
    {
	my ($P_x, $P_y, $P_z) = getXYZ($coordsCA[$i]);
	$P = NewVec($P_x, $P_y, $P_z);

	$VL = $P1 - $P0; # Getting vector direction
	$W = $P - $P0;
	
	$product = $VL x $W;
	
	$magnitudeVLxW =  $product->Magnitude();
	$magnitudeVL = $VL->Magnitude();
	$dist = $magnitudeVLxW/$magnitudeVL;

	push (@distToLine, $dist);
	
    }
    return @distToLine;
}

	    
