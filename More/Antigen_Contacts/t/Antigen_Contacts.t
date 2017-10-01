#!/acrm/usr/local/bin/perl


# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl Antigen_Contacts.t'

#########################

use strict;
use warnings;
use Data::Dumper;
# change 'tests => 1' to 'tests => last_test_to_print';

#use lib("./lib");
#use lib ("/home/bsm/ucbterd/Desktop/Antigen_Contacts/lib");
use lib ("~/allscripts/lib");
use Antigen_Contacts qw(antibody_antigen_contacts);
use Antigen_Contacts qw(get_antigen_chain_id);
use Antigen_Contacts qw(antibody_cont_residue);
use Antigen_Contacts qw(antigen_cont_residue);
#use Antigen_Contacts qw(is_consective);
use Test::Simple (tests => 4);
#BEGIN { use_ok('Antigen_Contacts') };

#########################
# Insert your test code below, the Test::More module is use()ed here so read
# its man page ( perldoc Test::More ) for help writing this test script.

my $pdb_file = "3PJS_1.pdb";
my ($array_AREF, $chains_HREF) = antibody_antigen_contacts($pdb_file);
my @array = @{$array_AREF};
my %chains = %{$chains_HREF};


ok(@array, "I GOT it");
print "@array\n";
print Dumper (\%chains);




#print "$array\n";
my $antigen = get_antigen_chain_id($pdb_file);
ok($antigen, "I am Antigen");

print "$antigen\n";
my $ann_arr;
my ($light_conts, $heavy_conts, $light_chain_conts, $heavy_chain_conts) = &antibody_cont_residue($pdb_file);
ok ($light_conts, "My Antibody contacts residues");
foreach  $ann_arr (@{$light_conts}){
    print "@{$ann_arr}", "\n";
    #print $$ann_arr[1];
   
}
# "@{$light_conts}\n";
foreach  my $ann_arr2 (@{$heavy_conts}){
    print "@{$ann_arr2}", "\n";

}

#print "@@{$heavy_conts}\n";

print Dumper ($light_chain_conts);
print Dumper ($heavy_chain_conts);


my ($ag_label, $antigen_conts, $antigen_contsss) = &antigen_cont_residue($pdb_file);

ok ($antigen_conts, " I Got Antigen contact residues");
print "$ag_label\n";
#print "@{$antigen_conts}\n";
foreach  my $ann_arr2 (@{$antigen_conts}){
    print "@{$ann_arr2}", "\n";

}


print Dumper ($antigen_contsss);


my @curr_frag = qw(72);
my @cons_array = qw (72 74 75 76 78 95 96);

my $fragment;

foreach my $ele (@cons_array){

 $fragment = is_consective(\@curr_frag, $ele);
}
print "@$fragment\n";
