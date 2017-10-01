use strict;
#use warnings;

# This script finds the distance of each CA on straight line (Regression line )
# i.e: Line of best fit

use Data::Dumper;
use SFPerlVars;
#use Math::Vector::Real;
use Math::Vec qw(NewVec);
my $pymol = "/usr/bin/pymol";
#my $pymol = "/acrm/usr/local/bin/pymol";


#chdir "/acrm/data/people/saba/data/dataNew/NR_Complex_Martin/";

my $infile = "epitope_sequence-G3-CR3";
open (my $EPITOPE, '<', "./stats/$infile") or die
    "Can not open file";
my $pdb;
my @antigen_DNA;
my @antigen_protein;
my %parsedData;
my @distancesCalphaToLine;
my @AlldistToLine;
my @distToLine;
my $antigen_chain_label;
my %distances;
###

my $aligned_pdb ;
my ($antigen_chain_label, $antigen_chian_type);
my (@rangeRegion, @residueFragment) = ();

while (<$EPITOPE>)
{

    # Obtaining PDB file name, regions and fragments in variable
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
	       print "$pdb_file\n\n";


	    # Making directory
	    my ($pdb, $ext) = split('\.', $pdb_file);
	    mkdir $pdb;
	    chdir $pdb;
	    ###############

	    # To align the antibody structure around a center                 
            my $abalign = $SFPerlVars::abalign;
            $aligned_pdb = "aligned_".$pdb_file;
            `$abalign -k ../$pdb_file $aligned_pdb`;


	    # To obtain the range on each epitopic region i.e, start and end    
	    @rangeRegion = getRegionRange($regions);
	    # To obtain all residues of fragments
	    @residueFragment = getFragmentResidue($fragments);	    
	    #################

	    # To make pymol figure of antigen with highlighted regions in red and 
	    # fragments in green 
	    writepymolScript(\@rangeRegion, \@residueFragment,
                             $antigen_chain_label, $aligned_pdb, $pdb_file);
	    #################


	    # The array of arrays containing distances to straight line for
            # all epitopic regions in one antibody 
	    my @distancesCalphaToLine = getDistanceToStraightLine 
		(\@rangeRegion, \@dataArray, $antigen_chain_label, $pdb_file);

	    # Hash of arrays to store regions corresponding to an antibody
	    $distances{$pdb_file} = [@distancesCalphaToLine];
	    
	}	    
	
    }
    last;
   
}
################### Main Endssss ###############

#print Dumper (\%distances);
open (my $OUT, ">./$pdb/myDistances.txt") or
    die ("Can not open file for writing\n");

foreach my $key (keys %distances)
{

print $OUT "$key: ",  Dumper ($distances{$key});


}


########### Methods ###############

sub getDistanceToStraightLine
{
    my ($rangeRegionRef, $dataArrayRef, $antigen_chain_label, $pdb_file) = @_;
    my $coords;
    my @coordsCA = ();
    my ($P, $P0, $P1);
    my $count = 1;
    my @AlldistToLine = ();

    foreach my $record (@{$rangeRegionRef})
    {
#	my @AlldistToLine = ();
	my ($startRes, $endRes) = ($record->[0], $record->[1]);

	@coordsCA = getCoordsForRegion
	    ($dataArrayRef, $antigen_chain_label, $startRes, $endRes);
	`cp ../regression ./`;

	`./regression ../$pdb_file $startRes $endRes $antigen_chain_label >regression.pdb`;
	makePeptideFigure("regression.pdb", $count);
	open (my $IN, "regression.pdb") or die "Can not open file";

	my ($startP, $endP, $numOfAtoms) =getRegionsEndCoords($IN);
        # CA coordinates of region start and end


	my ($P0_x, $P0_y, $P0_z) = getXYZ($startP);
	my ($P1_x, $P1_y, $P1_z) = getXYZ($endP);

	# Making 3D vectors of points
	$P0 = NewVec($P0_x, $P0_y, $P0_z);
        $P1 = NewVec($P1_x, $P1_y, $P1_z);
	my @distToLine = calculateDistanceRegion ($P0, $P1, @coordsCA);

	push (@AlldistToLine, [@distToLine] );
	$count++;
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

sub getFragmentResidue
{
    my ($fragments) = @_;
    my (@splittedFragments, @fragments);
    @fragments = split (/,/, $fragments);

    foreach my $fragment (@fragments)
    {
        chomp $fragment;
        push (@splittedFragments, split (" ", $fragment) );
    }

    return  @splittedFragments;
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
=pod
sub getRegionsEndCoords
{
    my (@coordsCA) = @_;
    my ($startP, $endP);
    $startP= $coordsCA[0];
    $endP = pop @coordsCA;
    return $startP, $endP;

}
=cut
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
    for (my $i = 0; $i<= (scalar @coordsCA)-1 ; $i++)
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

	    
sub getRegionsEndCoords                                                                                            
{                      
    my ($regressionFileHandle) = @_;                                                                          
    my ($startP, $endP, $numOfAtoms); 
    my @file = <$regressionFileHandle>;
   
    my @regression;
    my $numOfAtoms = 1;
    my $xyzCoords = "";
    
    foreach my $line (@file)
    {
	if ($line =~ m/ATOM\s+\d+\s+(\w+)\s+LYS\s+X\s+\d+\s+(\S+\.\S+)\s+(\S+\.\S+)\s+(\S+\.\S+)\s+.+\..+\..+/ig)
	{
	    $xyzCoords = $2."\t".$3."\t".$4;
	    push(@regression, $xyzCoords);
	}
	$numOfAtoms++;
    }

    my $startP = $regression[0];
    my $endP = $regression[$#regression];
   
#print $startP, "\n";
#print $endP, "\n";
 
    return $startP, $endP, $numOfAtoms;                                                                                         
}  


sub writepymolScript
{
    my ($rangeRegionRef, $residueFragmentRef,
        $antigen_chain_label, $aligned_pdb, $pdb_file) = @_;

    open (my $PYMOL, '>', "pymolscript.pml") or die
        "Can not open file"; # opening perl script file                                                                         

    print {$PYMOL} "load $aligned_pdb\n";
    print {$PYMOL} "bg_color white\n";
    print {$PYMOL} "turn x, 90\n";
    print {$PYMOL} "turn y, 90\n";
    print {$PYMOL} "turn x, 90\n";
    print {$PYMOL} "turn y, 90\n";
    print {$PYMOL} "show cartoon\n";
    print {$PYMOL} "hide lines\n";
    print {$PYMOL} "select light, chain L\n";
    print {$PYMOL} "remove light\n";
    print {$PYMOL} "select heavy, chain H\n";
    print {$PYMOL} "remove heavy\n";
    print {$PYMOL} "color cyan, chain $antigen_chain_label\n";

    foreach my $record (@{$rangeRegionRef})
    {

        print {$PYMOL} "select regions, resi $record->[0]-$record->[1]\n";
        print {$PYMOL} "color red, regions\n";

    }

    foreach my $record (@{$residueFragmentRef})
    {

        print {$PYMOL} "select fragments, resi $record\n";
        print {$PYMOL} "color green, fragments\n";

    }


    print {$PYMOL} "ray 800,600\n";
    print {$PYMOL} "png $pdb_file.png\n";
    print {$PYMOL} "quit\n";

    `$pymol pymolscript.pml`;
#    unlink $PYMOL;
#    unlink $aligned_pdb;

}

sub makePeptideFigure
{
my ($PeptidePdbFile, $count) = @_;

open (my $PYMOL, '>', "Peppymolscript.pml") or die
    "Can not open file"; # opening perl script file 


print {$PYMOL} "load $PeptidePdbFile\n";
print {$PYMOL} "bg_color white\n";
print {$PYMOL} "turn x, 90\n";
print {$PYMOL} "turn y, 90\n";
print {$PYMOL} "turn x, 90\n";
print {$PYMOL} "turn y, 90\n";
print {$PYMOL} "show cartoon\n";

print {$PYMOL} "ray 800,600\n";
print {$PYMOL} "png $PeptidePdbFile$count.png\n";
print {$PYMOL} "quit\n";

`$pymol Peppymolscript.pml`;

  
}
