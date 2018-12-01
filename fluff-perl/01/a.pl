#!/usr/bin/perl

use warnings;
use strict;

my $freq = 0;
while(my $a = <STDIN>) {
    chomp($a);
    $freq += $a;
}

print $freq;
