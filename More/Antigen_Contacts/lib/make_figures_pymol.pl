#!/usr/bin/perl
# This script reads epitope_sequence.txt file and makes figures to show regions
# and fragments in the 3D structure of an antigen by using pymol. It writes 
# .pml (pymol script) with all the commands and then sends that pymol script to
# program pymol. 

use strict;
#use warnings;
use Data::Dumper;
use SFPerlVars;
my $pymol = "/usr/bin/pymol";
my @antigen_DNA;
my @antigen_protein;

chdir "/acrm/data/people/saba/data/dataNew/NR_Complex_Martin/";
my $infile = "epitope_sequence-G3-CR3";
open (my $EPITOPE, '<', "./stats/$infile") or die
    "Can not open file";

while (<$EPITOPE>)
{
    open (my $PYMOL, '>', "pymolscript.pml") or die
	"Can not open file"; # opening perl script file
    my ($pdb_file, $regions, $fragments) = split (':', $_);
    my (@regions, @fragments);
    if ($_ =~ /^Antibody/) # ignoring first line
    {
	next;
    }
    
    else
    {
	open (my $COMPLEX, $pdb_file) or die
	    "Can not open $pdb_file"; # open PDB file
	
	my $chaintype = $SFPerlVars::chaintype;
        # To obtain the antigen chain label and chain type (N Protein)
	my ($antigen_chain_label, $antigen_chian_type) = 
	    split(" ", `$chaintype $pdb_file | head -1`);
	
	if ( ($antigen_chian_type eq 'DNA') or 
	     (($antigen_chian_type eq 'RNA') ) )
        # To keep track of DNa antigens
	{
	    push (@antigen_DNA, $pdb_file);
	    next;
	}
	else
	{
	    push (@antigen_protein, $pdb_file);

        # To align the antibody structure around a center
	    my $abalign = $SFPerlVars::abalign;
	    my $aligned_pdb = "aligned_".$pdb_file;
	    `$abalign -k $pdb_file $aligned_pdb`;  
	    
	   # $regions =~ s/ |,/+/g;
	  # $fragments =~ s/ |,/+/g;
	    my (@splittedRegion, @splittedFragments);
	    my (@rangeRegion, @rangeFragment) = ();
	    my ($lastElement, $lastElementF);
	    @regions = split (/,/, $regions);
	    @fragments = split (/,/, $fragments);

	    foreach my $region(@regions)
	    {
		@splittedRegion = split (" ", $region);
		$lastElement = pop @splittedRegion;
		push ( @rangeRegion, [$splittedRegion[0], $lastElement ] )
	    }

	    foreach my $fragment (@fragments)
	    {
		@splittedFragments = split (" ", $fragment);
		$lastElementF = pop @splittedFragments;
	if($splittedFragments[0]==undef)
	{
	push ( @rangeFragment, [$lastElementF ] );
	}
	else
	{
#$splittedFragments[0],
                push ( @rangeFragment, [$splittedFragments[0], $lastElementF ] );
	}
	    }


	    chomp $regions;
	    chomp $fragments;
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
	    foreach my $record (@rangeRegion)
	    {
	
		print {$PYMOL} "select regions, resi $record->[0]-$record->[1]\n";
		print {$PYMOL} "color red, regions\n";
		
	    }
#	    print {$PYMOL} "color red, regions\n";
#	    print {$PYMOL} "select fragments, resi $fragments\n";
#	    print {$PYMOL} "color green, fragments\n";
	    foreach my $record (@rangeFragment)
	    {
		if (@$record == 1)
		{
		#print {$PYMOL} "select fragments, resi $record->[0]-$record->[1]\n";
		print {$PYMOL} "select fragments, resi $record->[0]\n";
		print {$PYMOL} "color green, fragments\n";
		}

		else
		{
		print {$PYMOL} "select fragments, resi $record->[0]-$record->[1]\n";		   
 		#print {$PYMOL} "select fragments, resi $record->[0]\n";
		    print {$PYMOL} "color green, fragments\n";
		}
	    
	    }


	    print {$PYMOL} "ray 800,600\n";
	    print {$PYMOL} "png $pdb_file.png\n";
	    print {$PYMOL} "quit\n";
	    
	    `$pymol pymolscript.pml`;
	    unlink $PYMOL;
	    unlink $aligned_pdb;   
	}
    }


}

if (-d "Figures")
{
  if (-d "$infile")
  {
    `mv *.png ./Figures/$infile`;
  }
  else 
  {
   mkdir "./Figures/$infile";
   `mv *.png ./Figures/$infile`;
  }

}

else
{
mkdir "Figures";
if (-d "$infile")
  {
    `mv *.png ./Figures/$infile`;
  }
  else 
  {
   mkdir "./Figures/$infile";
   `mv *.png ./Figures/$infile`;
  }

}


print scalar @antigen_DNA, " DNA Antigens are:  @antigen_DNA\n";
print scalar @antigen_protein, " Protein Antigens are: @antigen_protein\n";
 
# To remove temporary aligned PDB file
#`rm aligned*.pdb`;

# ***************************
