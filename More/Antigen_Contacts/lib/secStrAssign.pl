use strict;
use warnings;

use Data::Dumper;
use SFPerlVars;

my $pdbFilePath = "/acrm/data/xmas/pdb/pdb";
my $ext = ".xmas";
my $pdb_id = "1AFV";
my $pdbFile = $pdbFilePath.lc($pdb_id).$ext;

my $progPath = "/home/bsm/martin/scripts/xmastoss.pl";

my $antigenLabel = "A";


#my @antigen = `perl /home/bsm/martin/scripts/xmastoss.pl /acrm/data/xmas/pdb/pdb1afv.xmas | grep $antigenLabel`;
#my @antigen = `$progPath $pdbFile | grep $antigenLabel`;

my @antigen = `$progPath $pdbFile | grep $antigenLabel`;

my $start = 20;
my $end = 30;


foreach my $line (@antigen)
{
    chomp $line; 

    if ($line =~ m/$antigenLabel$start\s+/g)
    {

	if ($start<=$end)
	{

	    print $line, "\n";
	    $start++;	   
	    
	}
	
    }

}
