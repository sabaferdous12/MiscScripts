#!/usr/bin/perl 
#
#********************************************************************8
# Program:      seqFeaturesPng
# File:         seqFeaturesPng.pl
# Usage:        ./seqFeaturesPng.pl <fasta file> <features file> <output file>
#
# Date:         5 Nov 2014
# Function:     Displays user defined features of the sequence graphically.
# Author:       Avneet Saini
# Address:      Institute of Structural and Molecular Biology
#               Division of Biosciences
#               University College
#               Gower Street
#               London
#               WC1E 6BT
# Email:        avneetsaini@gmail.com
#******************************************************************************
# Description:  This script reads two input files, i) File containing sequence
#               in fasta format, ii) File containing features to be displayed,
#               in the foramt <primary_tag,tag,start_residue end_residue>, (for
#               example: catalytic domain,Cat1,40 80. This would be displayed in
#               the png file as a catalytic domain feature, labelled Cat1
#               starting from amino acid 40 and ending at residue 80.
# Output:       PNG file
#******************************************************************************
use strict;
use warnings;

use Bio::Graphics;
use Bio::SearchIO;
use Bio::SeqFeature::Generic;
use Bio::Seq;
use Bio::SeqIO;
use Data::Dumper;

# Inputs
my ($file1, $file2, $file3) = @ARGV;
my $argc = $#ARGV + 1;
if ($argc < 3)
{ 
    print "Usage: seqFeaturesPng.pl <fasta file> <features file> <output filename>\n\n";
    print "example: seqFeaturePng.pl desmin.fasta desmin.feature
     desmin.png\n";
}

# Gets the fasta sequence from the file 
my $search = Bio::SeqIO->new
    (
     -file   => $file1,
     -format => 'fasta'
    ) or die "parse failed";
my $seq= $search->next_seq or die "Ccould not find a sequence in the file";

# creates an object for the sequence over its entire length
my $wholeseq = Bio::SeqFeature::Generic->new
    (
    -start        => 1,
    -end          => $seq->length,
    -display_name => $seq->display_name,
    );

# Produce a png rendering of the sequence and its features
# Create a new panel over the entire length of the sequence, with a width of 800
# pixels, boundary around the left and right edges of the image that specify a
# 10 pixel leeway. Key labeling to be done along the image in between 

my $panel = Bio::Graphics::Panel->new
    (
     -length    => $seq->length,
     -width     => 800,
     -pad_left  => 10,
     -pad_right => 10,
     -key_style => 'between',
    );

# Add a new track to the panel that draws a scale all across the image 
$panel->add_track
    ($wholeseq,
     -glyph  => 'arrow',
     -bump   => 0,
     -double => 1,
     -tick   => 2
    );

# Add a new horizontal track to the panel that represents the sequence in red
$panel->add_track
    ($wholeseq,
     -glyph     => 'generic',
     -label     => 1,
     -bgcolor   => 'red',
     -fontcolor => 'blue',
    );

# get the features to be displayed
my $featureHash_ref = getFeatureHash($file2);
my %featureHash= %$featureHash_ref;

# Create the new features to be displayed on the sequence and label them by
# giving different primary_tags 
foreach my $primary_tag(keys %featureHash)
{
    print "$primary_tag:";
    for my $tag (keys %{$featureHash{$primary_tag}})
    {
	print "$tag=$featureHash{$primary_tag}{$tag}[0]-
                $featureHash{$primary_tag}{$tag}[1]\n";
	my $feature = Bio::SeqFeature::Generic->new
	    (
	     -display_name => $tag,
	     -start        => $featureHash{$primary_tag}{$tag}[0],
	     -end          => $featureHash{$primary_tag}{$tag}[1],
	     -primary_tag  => $primary_tag,
	    );
	$seq->add_SeqFeature($feature);
    }
}

my @sites = $seq->all_SeqFeatures; # array of features
 
# If more than one primary tags partition features by their primary tags
my %sorted_sites;
for my $f (@sites) 
{
    my $tag = $f->primary_tag;
    push @{$sorted_sites{$tag}},$f;
}

#specify colors for the various features
my @colors = qw(orange cyan green pink grey);  
my $idx    = 0;
for my $tag (sort keys %sorted_sites) 
{
    my $site = $sorted_sites{$tag};

    # Add track of various features to the image 
    $panel->add_track
	($site,
	 -glyph       =>  'generic',
	 -bgcolor     =>  $colors[$idx++ % @colors],
	 -fgcolor     => 'black',
	 -key         => "${tag}",
	 -bump        => +1,
	 -height      => 8,
	 -label       => 1,
	 -font2color  => 'red',
	 -description => sub 
	 {
	     my $feature = shift;
	     my $start   = $feature->start;
	     my $end     = $feature->end;
	     return "Residues:$start-$end";
	 },
	);
                  
}

#open output file to write
open (my $PNGFILE, "> $file3") or die "$file3 Error: $!\n"; 
print $PNGFILE $panel->png;




########################################################
sub getFeatureHash
{
    my ($featureFilename)=@_;
    my %featureDescriptionRef;
    open (my $FFILE, "< $featureFilename") or die "Error $featureFilename: $!";
    while (<$FFILE>)
    {
	chomp $_;
	my ($primary_tag, $tag, $featurePoints)= split(/,/, $_) ;
	my @points= split ' ', $featurePoints;
	$featureDescriptionRef{$primary_tag}{$tag}= [@points];
    }
    return (\%featureDescriptionRef);
}
