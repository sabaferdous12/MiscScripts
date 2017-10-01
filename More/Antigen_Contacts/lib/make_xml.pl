#!/usr/bin/perl 

# This Scripts read all the antibody complxes from current directory and writes
# an XML file for each complex
# Subroutines in module, Antigen_Contacts.pm
# USAGE: ./make_xml.pl                                           
# Author: Saba Ferdous                                                         
use strict;
use warnings;
use XML::DOM;
use Antigen_Contacts qw (antibody_antigen_contacts antibody_cont_residue 
antigen_cont_residue output_File_name read_dir);

# Reading direcoty files into an array
my $dir = ".";
my @dir_files = read_dir($dir);
foreach my $pdb_file (@dir_files){
#my $pdb_file = "1DEE_1.pdb";

# Opening files for writing new xml for each antibody complex
# Outout file name, antibody-complex_name_contacts.xml
my $pdb_xml_name =  output_File_name($pdb_file);
open (my $XML, '>', $pdb_xml_name);

# Subroutine calls - Reading data into arrays and hashes
my ($light_resi, $heavy_resi, $light_chain_conts, $heavy_chain_conts) 
    = antibody_cont_residue($pdb_file);
my ($antigen_chain_label, $antigen_resi, $antigen_chain_conts)
    = antigen_cont_residue($pdb_file);

# XML writing starts...
my $doc = XML::DOM::Document->new;
my $xml_pi = $doc->createXMLDecl ('1.0');
print {$XML} $xml_pi->toString;

my $root = $doc->createElement('Data');
my $complex = $doc->createElement('complex');
$root->appendChild($complex);

my $light_chain = $doc->createElement('light_chain');
$light_chain->setAttribute('chain_id', 'L');
$complex->appendChild($light_chain);

my $l_contacts= $doc->createElement('contacts');
$light_chain->appendChild($l_contacts);

foreach my $l_res (sort keys (%{$light_chain_conts})){
my $residue = $doc->createElement ('residue');
$residue->setAttribute('id', 'L'.$l_res);
$residue->setAttribute('residue_cont_num', $$light_chain_conts{$l_res});
$l_contacts->appendChild($residue);
}

my $heavy_chain = $doc->createElement('heavy_chain');
$heavy_chain->setAttribute('chain_id', 'H');
$complex->appendChild($heavy_chain);

my $h_contacts = $doc->createElement('contacts');
$heavy_chain->appendChild($h_contacts);

foreach my $h_res (sort keys (%{$heavy_chain_conts})){
    my $h_residue = $doc->createElement ('residue');
    $h_residue->setAttribute('id', 'H'.$h_res);
    $h_residue->setAttribute('residue_cont_num', $$heavy_chain_conts{$h_res});
    $h_contacts->appendChild($h_residue);
}

my $antigen_chain = $doc->createElement('antigen_chain');
$antigen_chain->setAttribute('chain_id', $antigen_chain_label);
$complex->appendChild($antigen_chain);

my $ag_contacts = $doc->createElement('contacts');
$antigen_chain->appendChild($ag_contacts);

foreach my $a_res (sort {$a<=>$b}keys (%{$antigen_chain_conts})){
    my $a_residue = $doc->createElement('residue');
    $a_residue->setAttribute('id',$a_res);
    $a_residue->setAttribute('residue_cont_num', $$antigen_chain_conts{$a_res});
    $ag_contacts->appendChild($a_residue);
}

print {$XML} $root->toString; 

}
