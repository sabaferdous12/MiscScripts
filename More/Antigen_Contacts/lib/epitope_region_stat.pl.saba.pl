use strict;
use warnings;
use Data::Dumper;
use Antigen_Contacts qw (antibody_antigen_contacts antibody_cont_residue                                                             
antigen_cont_residue output_File_name read_dir get_hash_key get_regions);

# Reading direcoty files into an array                             
my $dir = ".";
my @dir_files = read_dir($dir);

# Open 2 files for writing, 1) For stats 2) For details
open(my $STAT, '>>', "epitope-stat.txt") or die "File can not open";
open(my $EPITOPE_REGIONS, '>>', "epitope_sequence.txt") or die "File Can not open";


print {$STAT} "Antibody:\tRegions:\tOdd_Bits\n";
print {$EPITOPE_REGIONS} "Antibody:\tRegions:\tOdd_Bits\n";
#print {$EPITOPE_REGIONS} "$pdb_file:\n";
foreach my $pdb_file (@dir_files) {
#my @all_frags = ([]);
#my $pdb_file = "1AFV_1.pdb";
my @fragments = ( [] );

my ($antigen_chain_label, $antigen_resi, $antigen_chain_conts)
    = antigen_cont_residue($pdb_file);

 #   my $current_frag_ref;
my @antigen_reseq = get_hash_key($antigen_chain_conts);
    
@fragments = get_regions(\@antigen_reseq);
#print "$pdb_file: \n";
my $count_regions = 0;
my $count_odd_bits = 0;
#print {$EPITOPE_REGIONS} "$pdb_file:\n";
#print "Antibody:\tRegions:\tOdd_Bits\n";
my @regions = ([]);
my @odds = ([]);

    foreach my $arr (@fragments){
	if (@$arr>=3){
	    $count_regions++;
	    push (@regions, $arr);
	    #print {$EPITOPE_REGIONS} "@$arr\n";
	    
	}
	else{
	    $count_odd_bits++;
	    push (@odds, $arr);
	    #print {$EPITOPE_REGIONS} "@$arr\n";	    
#print "@$arr\n";
    }

#	print {$EPITOPE_REGIONS}
}

print {$EPITOPE_REGIONS} "$pdb_file:\t@regions:\t@odds\n";
print {$STAT} "$pdb_file:\t$count_regions\t\t$count_odd_bits\n";   
 #   last;
}

=pod
foreach my $arr (@all_frags){                                                     
    print "$arr\n"; 
                                                           
} 

#print "@fragments\n";
