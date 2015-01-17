# -*- Perl -*-
#
# A module for the generation or analysis of counterpoint.
#
# NOTE still being written so may change, a lot.
#
# Run perldoc(1) on this file for additional documentation.

package Music::Counterpoint;

use 5.10.0;
use strict;
use warnings;

use List::Util qw/min shuffle/;
use Moo;
use namespace::clean;
use Scalar::Util qw/looks_like_number/;

our $VERSION = '0.01';

##############################################################################
#
# CONSTANTS

# The principal voice against which the other voices will be developed; this is
# called the "cantus firmus" (sometimes hereafter "c.f."). Counterpoint rules
# have a $voice that may be filled in with a particular voice number under
# consideration, or the rule may need delve through the complete $cantus
# reference, for example when looking for parallel fifths.
sub FIRMUS () { 0 }

# For whether or not _getpitch() will recurse to find a note or give up; some
# rules will need to consider suspensions and passing tones, while others might
# not apply.
sub NODELVE () { 0 }
sub DELVE ()   { 1 }

# Intervals, in semitones.
sub UNISON () { 0 }
# Perfect 4th, e.g. the interval from C to F, which is otherwise five
# semitones. There has been a fairly long debate over whether this interval is
# dissonant or not, the gist of which being "it depends."
sub P4 () { 5 }
# The tritone might be abbreviated as TT in the docs.
sub TRITONE () { 6 }
sub P5 ()      { 7 }
sub OCTAVE ()  { 12 }

##############################################################################
#
# ATTRIBUTES

# Lol! Where FIRMUS is the melody, and any other lists the other voices.
has _cantus => (
  is        => 'rw',
  clearer   => 1,
  predicate => 1,
);

# Rules that apply anywhere in a phrase. More common rules (or those less
# expensive) probably should head the list.
has _any_rules => (
  default => sub { [] },
  is      => 'rw',
);
# Rules that apply at a particular location, array index position in cantus,
# negatives supported so -1 can target the end of the melody). List of rules
# otherwise, under the position number key.
has _at_rules => (
  default => sub { {} },
  is      => 'rw',
);

# Voice metadata, hash of hash by voice index number; stores presumably such
# things as pitch ranges for the voice, what mode the voice is in, etc.
has _vmd => (
  is      => 'rw',
  default => sub { {} },
);

##############################################################################
#
# METHODS

sub BUILD {
  my ( $self, $param ) = @_;
  $self->melody( $param->{melody} ) if exists $param->{melody};
}

# utility method for generate()
sub _candidate_pitches {
  my ($pitch) = @_;
  # TODO improve this, e.g. with vmd limits, etc.
  my @pitches = shuffle $pitch - 24 .. $pitch + 24;
  return \@pitches;
}

sub add_rules {
  my $self = shift;
  die "must add at least one rule" if !@_;
  push @{ $self->_any_rules }, @_;
  return $self;
}

sub add_rule_at {
  my ( $self, $at, $rule ) = @_;
  die "position must be a number" if !defined $at or !looks_like_number $at;
  die "must specify a rule" if !defined $rule;
  push @{ $self->_at_rules->{ int $at } }, $rule;
  return $self;
}

# If get around to having well-known sets of rules, e.g. for counterpoint in a
# particular style.
#sub add_ruleset {
#  my ( $self, $ruleset ) = @_;
#  ...;
#  return $self;
#}

sub add_voice {
  my $self = shift;
  my $vmd = shift // {};
  $_[0] = [] if !defined $_[0];

  die "no melody specified" if !$self->_has_cantus;

  # see perldocs on Storable if you instead need a copy
  my $voice = push @{ $self->_cantus }, ref $_[0] eq 'ARRAY' ? $_[0] : \@_;
  $voice--;    # count to last index
  $self->_vmd->{$voice} = $vmd;

  return $self;
}

#sub analyze {
#  my ($self) = @_;
#  # analyze might work on a melody-only phrase, assuming there are rules that
#  # apply to only one voice loaded. Might want to screen rules against the
#  # number of voices they apply to, so if only have a melody, only run the
#  # single-voice rules?
#  die "no melody specified" if !$self->_has_cantus;
#  die "no rules have been set"
#    if !@{ $self->_any_rules } and !%{ $self->_at_rules };
#
#  ...;
#}

sub clear_cantus {
  my ($self) = @_;
  $self->_clear_cantus;
  $self->_vmd( {} );
  return $self;
}

sub _apply_rules {
  my ( $self, $rulesref ) = @_;

}

sub generate {
  my ($self) = @_;
  die "no melody specified" if !$self->_has_cantus;
  die "generate requires at least two voices" if @{ $self->_cantus } < 2;
  die "no rules have been set"
    if !@{ $self->_any_rules } and !%{ $self->_at_rules };

  my $cantus = $self->_cantus;

  # TODO obviously support more... (and TODO do not clobber a voice that is
  # already populated, etc. Also worry about "undef" as in unset note vs.
  # how will indicate the note is a rest, sigh)
  my $voice = 1;

  my %fails;
  my $curidx   = 0;
  my $done     = 0;
  my $possible = _candidate_pitches( $cantus->[FIRMUS][$curidx] );

ITER: while ( !$done ) {
    $cantus->[$voice][$curidx] = shift @$possible;

    # Whoops, ran out of possible notes at this position, backtrack and try
    # something else. Toss exception if cannot backup more.
    if ( !defined $cantus->[$voice][$curidx] ) {
      die "unable to generate counterpoint" if $curidx == 0;
      delete $fails{$curidx};
      $curidx--;
      $fails{$curidx}->{ $cantus->[$voice][$curidx] }++;
      $possible = _candidate_pitches( $cantus->[FIRMUS][$curidx] );
      next ITER;
    }

    next ITER if exists $fails{$curidx}->{ $cantus->[$voice][$curidx] };

    my @rulesrefs = $self->_any_rules;
    if ( exists $self->_at_rules->{$curidx} ) {
      unshift @rulesrefs, $self->_at_rules->{$curidx};
    } elsif ( exists $self->_at_rules->{ $curidx - @{ $cantus->[FIRMUS] } } ) {
      unshift @rulesrefs, $self->_at_rules->{ $curidx - @{ $cantus->[FIRMUS] } };
    }

    # TODO collect metrics on what rules pass/fail, return that somehow
    # also probably need some way to rank various rules, e.g. in multi-voice
    # setups where certain rules are relaxed, though that code should be
    # distinct from the rule, to keep those simpler?
    for my $rr (@rulesrefs) {
      for my $rule (@$rr) {
        my ( $status, $msg ) =
          $rule->{assert}->( $cantus, undef, $curidx, FIRMUS, $voice );
        # undef means "no opinion" e.g. rule does not apply
        next if !defined $status;
        $status = !$status if $rule->{invert};
        if ( !$status ) {
          $fails{$curidx}->{ $cantus->[$voice][$curidx] }++;
          next ITER;
        }
      }
    }

    $curidx++;
    if ( $curidx > $#{ $cantus->[FIRMUS] } ) {
      $curidx = $#{ $cantus->[FIRMUS] };
      $done   = 1;
    } else {
      $possible = _candidate_pitches( $cantus->[FIRMUS][$curidx] );
    }
  }

  return $self;
}

sub melody {
  my $self = shift;
  die "no melody specified" if !defined $_[0];

  # see perldocs on Storable if you instead need a copy
  $self->_cantus( ref $_[0] eq 'ARRAY' ? [ $_[0] ] : [ \@_ ] );

  return $self;
}

sub remove_voice {
  my ( $self, $voice ) = @_;
  my $cantus = $self->_cantus;

  if ( !defined $voice ) {
    die "cannot remove melody" if @$cantus <= 1;
    $voice = $#$cantus;
    pop @$cantus;
  } else {
    die "voice must be a number" if !looks_like_number $voice;
    die "voice outside of available voices" if $voice < 1 or $voice > $#$cantus;
    splice @$cantus, $voice, 1;
  }
  delete $self->_vmd->{$voice};

  return $self;
}

##############################################################################
#
# RULES
#
# These are functions that return code references and may optionally accept
# arguments that set variables used by the code references, e.g. a list of
# allowed intervals, or the like. This allows the rules generation phase to set
# certain variables based on the cantus firmus, and then the rules checking
# phase to call the code references that determine whether the counterpoint is
# valid. As such, rules must conform to particular expected inputs and outputs.

# Rule utility method. Returns a pitch from presumably a $cantus voice
# reference, which is a list of array references.
sub _getpitch {
  my ( $what, $candelve ) = @_;
  if ( ref $what eq 'ARRAY' ) {
    return if $candelve == NODELVE;
    return _getpitch( $what->[0], DELVE );
  } else {
    return $what;
  }
}

# True if the interval between the given voices is any of those specified in
# semitones.
sub absinterval {
  my @intervals = @_;
  return sub {
    my ( $cantus, $vmd, $index, $v1, $v2 ) = @_;
    my $v1n = _getpitch( $cantus->[$v1][$index], NODELVE );
    my $v2n = _getpitch( $cantus->[$v2][$index], NODELVE );
    return undef, "no match" if !defined $v1n or !defined $v2n;
    my $icur = abs( $v2n - $v1n );
    for my $i (@intervals) {
      return 1, "ok" if $icur == $i;
    }
    return 0, "interval not found";
  };
}

# "4ths between root voice and next up"; suspensions and passing tone cases
# presumably handled elsewhere. Second inversion chords <g c e> get all sorts
# of hate in counterpoint, as this puts the (perhaps) dissonant interval in
# the root.
sub rootfourth {
  return sub {
    my ( $cantus, $vmd, $index, $v1, $v2 ) = @_;
    my @pitches = sort { $a <=> $b } map {
      my $p = _getpitch( $cantus->[$_][$index], NODELVE );
      defined $p ? $p : ()
    } 0 .. $#$cantus;
    return undef, "no match"    if @pitches < 2;
    return 1,     "root fourth" if $pitches[1] % 12 - $pitches[0] % 12 == P4;
    return 0,     "ok";
  };
}

sub parallel {
  my @intervals = @_;
  return sub {
    my ( $cantus, $vmd, $index, $v1, $v2 ) = @_;

    return undef, "not enough notes" if $index < 1;

    my $v1n = _getpitch( $cantus->[$v1][$index], NODELVE );
    my $v2n = _getpitch( $cantus->[$v2][$index], NODELVE );

    my $v1np = _getpitch( $cantus->[$v1][ $index - 1 ], NODELVE );
    my $v2np = _getpitch( $cantus->[$v2][ $index - 1 ], NODELVE );

    return undef, "no match"
      if !defined $v1n
      or !defined $v2n
      or !defined $v1np
      or !defined $v2np;

    my $icur  = abs( $v2n - $v1n );
    my $iprev = abs( $v2np - $v1np );
    return 0, "not parallel" if $icur != $iprev;

    for my $i (@intervals) {
      return 1, "parallel interval $i" if $icur == $i;
    }
    return 0, "ok";
  };
}

# Is the root (lowest) note a particular value? (avoids new voice below
# changing the harmony, e.g. at beginning and end of a phrase).
sub root_is {
  my ($root_pitch) = @_;
  return sub {
    my ( $cantus, $vmd, $index, $v1, $v2 ) = @_;
    my @pitches = sort { $a <=> $b } map {
      _getpitch( my $p = $cantus->[$_][$index], NODELVE );
      defined $p ? $p : ()
    } 0 .. $#$cantus;
    return undef, "no match"            if !@pitches;
    return 1,     "root is $root_pitch" if $pitches[0] == $root_pitch;
    return 0,     "ok";
  };
}

# voice test, that a melody conforms to particular intervals (tritones may need
# more nuanced tests; Norden holds that tritone leaps should ideally be
# compensated by a move in the opposite direction, e.g. f down to b up to c).
sub voice_absinterval {
  my @intervals = @_;
  return sub {
    my ( $cantus, $vmd, $index, $v1, $v2 ) = @_;
    return undef, "not enough notes" if $index < 1;

    # NOTE having to use $v2 here as c.f. is in $v1 is a bit surprising,
    # maybe melody rules need to have a different call interface?
    my $cur  = _getpitch( $cantus->[$v2][$index],       NODELVE );
    my $prev = _getpitch( $cantus->[$v2][ $index - 1 ], NODELVE );
    return undef, "no match" if !defined $cur or !defined $prev;

    my $icur = abs( $cur - $prev );
    for my $i (@intervals) {
      return 1, "ok" if $icur == $i;
    }
    return 0, "no match";
  };
}

1;
__END__

##############################################################################
#
# DOCS

=pod

=head1 NAME

Music::Counterpoint - rule-based generation and analysis of counterpoint

=head1 SYNOPSIS

NOTE under development, details will change, blah blah blah

  my $cpt = Music::Counterpoint->new;

  # A melody must be set before using subsequent calls
  $cpt->melody(qw/0 4 2 0 5 4 7 5 4 2 0/);

  # One or more voices must then be specified; here, an empty voice
  $cpt->add_voice;

  # Then, add rules:
  # tho Music::Counterpoint::Rules::noparallel might be annoying to type,
  # and ns pollution otoh stupid. maybe MCR->new then $rule->get or smth?
  # or use a config file, and then code constructs whatever is necessary...
  $cpt->add_rules(
    ...
  );

  # Then, the counterpoint can be generated,
  $cpt->generate();

=head1 DESCRIPTION

A module for the generation or analysis of counterpoint. A melody must be
supplied, and other voices specified. Rules must then be setup, and then either
the melody with voices tested, or additional voices generated in counterpoint
with the main melody and each other.

=head1 METHODS

Method calls will throw exceptions on invalid input, and especially if
generation or analysis is attempted prior to setting a B<melody> and then
adding some voices. Methods contrast with the L</"RULES"> used to construct or
confirm the counterpoint.

=over 4

=item B<add_rule_at>(I<position>, I<rule>)

Add a rule at melody index I<position>. This position may use the negative form
(e.g. C<-1>, C<-2>) to indicate end-of-melody positions.

=item B<add_rules>(I<rule>, ...)

Adds the specified rules. These will be tested at every position in the melody.

=item B<add_voice>(I<vmd>, I<pitch numbers>)

Adds a voice, with an optional hash reference specifying "voice metadata" and
an optional array reference or list specifying a melody for the given voice.
Must be called after a B<melody> has been set.

  $cpt->add_voice()                  # new empty voice
  $cpt->add_voide({ fixme => 1 })    # empty voice with metadata
  $cpt->add_voide(undef, qw/0 4 2/)  # no metadata, notes specified

=item B<clear_cantus>

Resets the internal structure containing the cantus firmus (set by B<melody>)
and other voices (set by B<add_voice>), and also wipes out any voice metadata.

=item B<generate>

Creates counterpoint, assuming a B<melody> has been loaded, and one or more
empty voices added to the cantus. Various rules should be setup before calling
this method. Will throw an exception if the counterpoint cannot be completed,
for example due to the rules being too strict.

=item B<new>

Creates a new C<Music::Counterpoint> object. This object must be passed a
melody before other methods are used. This can be done either the B<melody>
method, or the I<melody> parameter to B<new>.

  my $cpt = Music::Counterpoint->new(melody => ...);
  $cpt->melody(...);

=item B<melody>(I<pitch numbers>)

Sets the melody or cantus firmus of the counterpoint. Returns the
C<Music::Counterpoint> object, so may be chained with other method calls.

The array reference or list is used directly, so any subsequent changes to
that data will affect what the object sees. Use C<dclone> of L<Storable> or
the like to create a distinct copy of the data to pass to B<melody> if this is
a problem.

=item B<remove_voice>([I<voice>])

Removes the last or if specified the specified voice.

=back

=head1 RULES

Rules are functions that return code references; the code references must TODO
probably will be moved to ::Rules or somesuch.

=over 4

=item absinterval

=item parallel

=item root_is

=item rootfourth

=item voice_absinterval

=back

=head1 SEE ALSO

My various other music modules, e.g. L<Music::AtonalUtil>, L<Music::Canon> and
so forth. Relevant books:

=over 4

=item *

"Fundamental Counterpoint" by Hugo Norden

=item *

That Fux book.

=back

=head1 BUGS

Please report any bugs or feature requests to C<bug-music-counterpoint at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Music-Counterpoint>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.

=head1 LICENSE

Copyright 2015 Jeremy Mates.

This program is free software; you can redistribute it and/or modify it
under the terms of the the Artistic License (2.0).

=cut
