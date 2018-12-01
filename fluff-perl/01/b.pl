#!/usr/bin/perl

use warnings;
use strict;

my @history;
my $freq = 0;
my @freqs;
while(my $a = <STDIN>) {
    chomp($a);
    push @freqs, $a;
}

while(1) {
    foreach my $a (@freqs) {
	if(grep {$_ == $freq} @history) {
	    print "We've reached $freq before\n";
	    exit;
	}
	push @history, $freq;
	$freq += $a;
    }
}
