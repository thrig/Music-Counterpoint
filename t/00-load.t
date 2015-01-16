#!perl
use 5.10.0;
use strict;
use warnings FATAL => 'all';
use Test::More;

plan tests => 1;

BEGIN {
  use_ok('Music::Counterpoint') || print "Bail out!\n";
}

diag("Testing Music::Counterpoint $Music::Counterpoint::VERSION, Perl $], $^X");
