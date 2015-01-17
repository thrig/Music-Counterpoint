#!perl

use strict;
use warnings;

use Test::More;    # plan is down at bottom
use Test::Exception;

eval 'use Test::Differences';    # display convenience
my $deeply = $@ ? \&is_deeply : \&eq_or_diff;

use Music::Counterpoint;

##############################################################################
#
# Basic interface
#
# Note that per the usual Perl conventions methods beginning with an underscore
# (e.g. _cantus) are for internal module use only, and must not be called from
# external code.

my $cpt           = Music::Counterpoint->new;
my @cantus_firmus = qw(0 4 2 0 5 4 7 5 4 2 0);

# can we set and remove the melody?
ok( !$cpt->_has_cantus, "cantus not set" );
isa_ok( $cpt->melody(@cantus_firmus), 'Music::Counterpoint' );
$deeply->( $cpt->_cantus, [ \@cantus_firmus ], "melody stored as lol" );
ok( $cpt->_has_cantus, "cantus is set" );
$cpt->clear_cantus;
ok( !$cpt->_has_cantus, "cantus not set" );

dies_ok( sub { $cpt->add_voice }, "no new voices allowed if no melody set" );

# can also pass melody in as array ref...
isa_ok( $cpt->melody( \@cantus_firmus ), 'Music::Counterpoint' );
$deeply->( $cpt->_cantus, [ \@cantus_firmus ], "melody also stored as lol" );

# voice stack fiddling
$cpt->add_voice;
$deeply->( $cpt->_cantus, [ \@cantus_firmus, [] ], "melody and voice" );

$cpt->remove_voice;
$deeply->( $cpt->_cantus, [ \@cantus_firmus ], "melody minus voice" );

$cpt->add_voice( { fixme => "foo" } );
# first voice is at idx 1 because of the melody at 0 (FIRMUS)
$deeply->( $cpt->_vmd->{1}, { fixme => "foo" }, "vmd set 1" );

$cpt->add_voice( { fixme => "bar" } );
$deeply->( $cpt->_vmd->{2}, { fixme => "bar" }, "vmd set 2" );

$cpt->remove_voice;
ok( !$cpt->_vmd->{2}, "vmd 2 not set" );
$cpt->remove_voice;
ok( !$cpt->_vmd->{1}, "vmd 1 not set" );

# or set melody by param to constructor...
$cpt = Music::Counterpoint->new( melody => \@cantus_firmus );
$deeply->( $cpt->_cantus, [ \@cantus_firmus ], "melody still stored, lol" );

dies_ok( sub { $cpt->generate }, "generate requires 2+ voices" );
$cpt->add_voice;

dies_ok( sub { $cpt->generate }, "generate requires rules to be set" );

$cpt->add_rules(
  # musically unsound, testing approved
  { name   => 'pfifths',
    desc   => 'only parallel fifths',
    assert => Music::Counterpoint::parallel(7),
  },
  { name   => 'absint',
    desc   => 'no big intervals',
    assert => Music::Counterpoint::voice_absinterval( 0 .. 7 ),
  },
);

$cpt->add_rule_at( 0, { name   => 'staticroot',
   desc   => 'no undercutting of melody',
   assert => Music::Counterpoint::root_is(0),
});
$cpt->add_rule_at( -1, { name   => 'staticroot',
   desc   => 'no undercutting of melody',
   assert => Music::Counterpoint::root_is(0),
});

$cpt->generate();

$deeply->(
  $cpt->_cantus,
  [ \@cantus_firmus, [ map { $_ + 7 } @cantus_firmus ] ],
  "horrible counterpoint"
);

plan tests => 18;
