package Log::Easy::Filter;
our $this_package;
our ($FILTER_REGEX, $NOT_FILTER_REGEX, $FILTER_ALL_REGEX, $MATCH_LOG_LEVEL_REGEX, $FILTER, $NOT_FILTER, $FILTER_CNT );
#  if any $(.*)log->write(...)'s are in the calling code, and the log level is specified with one of the followin prepended with a '$'
use constant DEFAULT_FILTER =>  (qw( mll lll cll qll ell all wll nll ill dll tll sll ), map { "dl$_" } ( 0 .. 9, 99 ));
use constant LOG_LEVELS     =>  ( map { $_ => "D_$_"; } qw( MESSAGE LOUD CLEAN QUIT EXIT
                                                            EMERG ALERT CRIT FATAL FAIL ERROR WARN NOTICE INFO DEBUG
                                                            TRACE SPEW ), map { "DEBUG$_"} ( 0 .. 9, 99 )
                                );

BEGIN {
  sub space {
    my $sp = 'space';
    # this is a stupid little subroutine to nicely display stuff
    # could probably use a format specifier or sprintf better, but what the hell
    # I mostly use this for aligning output nicely
    my $piece     = shift;
    my $max       = shift;
    defined $max or $max = 27;
    my $separator = shift;
    unless( defined $separator and length $separator > 0 ) {
      $separator  = ' ';
    }
    my $lp = defined $piece ? length $piece : 0;
    my $ls = length $separator;
    my $multiplier = $lp < $max ? int (( $max - $lp )/$ls ) : 0;
    my $spacer = $separator x $multiplier;
    my $return = $sp eq 'space' ? $piece . $spacer : $spacer . $piece;
    my $lr = length $return;
    my $finisher = $lr < $max ? ( ' ' x ( $max - $lr )) : '';
    $return = $sp eq 'space' ? ( $return . $finisher ) : ( $finisher . $return );
    return $return;
    
  }
  sub pad {
    my $sp = 'pad';
    # this is a stupid little subroutine to nicely display stuff
    # could probably use a format specifier or sprintf better, but what the hell
    # I mostly use this for aligning output nicely
    my $piece     = shift;
    my $max       = shift;
    defined $max or $max = 27;
    my $separator = shift;
    unless( defined $separator and length $separator > 0 ) {
      $separator  = ' ';
    }
    my $lp = defined $piece ? length $piece : 0;
    my $ls = length $separator;
    my $multiplier = $lp < $max ? int (( $max - $lp )/$ls ) : 0;
    my $spacer = $separator x $multiplier;
    my $return = $sp eq 'space' ? $piece . $spacer : $spacer . $piece;
    my $lr = length $return;
    my $finisher = $lr < $max ? ( ' ' x ( $max - $lr )) : '';
    $return = $sp eq 'space' ? ( $return . $finisher ) : ( $finisher . $return );
    return $return;
    
  }
  sub space_pad {
    my $sp = pop;
    # this is a stupid little subroutine to nicely display stuff
    # could probably use a format specifier or sprintf better, but what the hell
    # I mostly use this for aligning output nicely
    my $piece     = shift;
    my $max       = shift;
    defined $max or $max = 27;
    my $separator = shift;
    unless( defined $separator and length $separator > 0 ) {
      $separator  = ' ';
    }
    my $lp = defined $piece ? length $piece : 0;
    my $ls = length $separator;
    my $multiplier = $lp < $max ? int (( $max - $lp )/$ls ) : 0;
    my $spacer = $separator x $multiplier;
    my $return = $sp eq 'space' ? $piece . $spacer : $spacer . $piece;
    my $lr = length $return;
    my $finisher = $lr < $max ? ( ' ' x ( $max - $lr )) : '';
    $return = $sp eq 'space' ? ( $return . $finisher ) : ( $finisher . $return );
    return $return;
    
  }
  
#  sub space {
#    space_pad( @_, 'space');
#  }
#  sub pad {
#    space_pad( @_, 'pad');
#  }


  
  #die;
  $this_package = __PACKAGE__;
  $ENV{LOG_PACKAGES_DEBUG} ||= 0;
  $ENV{LOG_FILTER_DEBUG}          = exists $ENV{LOG_FILTER_DEBUG}          ? $ENV{LOG_FILTER_DEBUG}          : 0;
  $ENV{LOG_FILTER_PACKAGES_DEBUG} = exists $ENV{LOG_FILTER_PACKAGES_DEBUG} ? $ENV{LOG_FILTER_PACKAGES_DEBUG} : 0; # want to make it so you can see what the filter is doing for specified packages only
  $ENV{LOG_FILTER}         ||= 'ON';
  $ENV{LOG_INTERNAL_DEBUG} ||= 0;
  print STDERR "THIS_PACKAGE=$this_package\n" if $ENV{LOG_FILTER_DEBUG};
  my @DEFAULT_FILTER= DEFAULT_FILTER();
  unless ( defined $FILTER_REGEX ) {
    my $FILTER;
    if( $ENV{LOG_FILTER} =~ /^off$/i ) {
      print STDERR __PACKAGE__, ":", __LINE__, ": ", "FILTER: IS OFF\n" if $ENV{LOG_FILTER_DEBUG};
      $FILTER = [];
      $NOT_FILTER = [ @DEFAULT_FILTER ];
    } elsif( $ENV{LOG_FILTER} =~ /^(on|\d+)$/i ) {
      print STDERR __PACKAGE__, ":", __LINE__, ": ", "FILTER: IS ON\n" if $ENV{LOG_FILTER_DEBUG};
      $FILTER = [ @DEFAULT_FILTER ];
      $NOT_FILTER = [];
    } else {
      print STDERR __PACKAGE__, ":", __LINE__, ": ", "FILTER: IS SPECIAL FILTER=$ENV{LOG_FILTER}\n" if $ENV{LOG_FILTER_DEBUG};
      my %not_filter = ();
      my %filter = ();
      foreach my $piece ( split( /:/, $ENV{LOG_FILTER} )) {
        if ( $piece =~ /^\!/ ) {
          $piece =~ s/^\!//;
          $not_filter{$piece} = $piece;
        } else {
          $filter{$piece}     = $piece;
        }
      }
      print STDERR __PACKAGE__, ":", __LINE__, ": ", "\%filter:     ", scalar keys %filter    , ":", join( ', ', keys %filter     ), "\n" if $ENV{LOG_FILTER_DEBUG};
      print STDERR __PACKAGE__, ":", __LINE__, ": ", "\%not_filter: ", scalar keys %not_filter, ":", join( ', ', keys %not_filter ), "\n" if $ENV{LOG_FILTER_DEBUG};
      if ( scalar keys %filter ) {
        $FILTER = [ keys %filter ];
        $NOT_FILTER = [];
      } else {
        #        $FILTER =     [ map { ($not_filter{$_} and $_ =~ /$not_filter{$_}/ )? () : $_; } ( @DEFAULT_FILTER ) ];
        #        $NOT_FILTER = [ map { ($not_filter{$_} and $_ =~ /$not_filter{$_}/ )? $_ : (); } ( @DEFAULT_FILTER ) ];
        my $not_filter_rx  = join('|', values %not_filter);
        $not_filter_rx = qr/$not_filter_rx/;
        print STDERR __PACKAGE__, ":", __LINE__, ": ", "not_filter_rx     : '$not_filter_rx'\n"  if $ENV{LOG_FILTER_DEBUG};
        $FILTER      = [ map { $_ =~ m/$not_filter_rx/ ? () : $_; } ( @DEFAULT_FILTER ) ];
        $NOT_FILTER  = [ map { $_ =~ m/$not_filter_rx/ ? $_ : (); } ( @DEFAULT_FILTER ) ];
      }
    }
    print STDERR __PACKAGE__, ":", __LINE__, ": ", "FILTER: ", join('|', @$FILTER ), "\n" if $ENV{LOG_FILTER_DEBUG};
    #$FILTER_REGEX = '\$[_a-zA-Z]*[_a-zA-Z0-9]*log->write\(.*?\$(' . join('|', @$FILTER) . '),.*?\);';
    $FILTER_REGEX = '\$[_a-zA-Z]+[_a-zA-Z0-9]*->write\(.*?\$(' . join('|', @$FILTER) . '),.*?\);';
    print STDERR __PACKAGE__, ":", __LINE__, ": ", "FILTER_REGEX     : $FILTER_REGEX\n"  if $ENV{LOG_FILTER_DEBUG};
    print STDERR __PACKAGE__, ":", __LINE__, ": ", "NOT_FILTER: ", join('|', @$NOT_FILTER ), "\n" if $ENV{LOG_FILTER_DEBUG};
    #$NOT_FILTER_REGEX = '\$[_a-zA-Z]*[_a-zA-Z0-9]*log->write\(.*?\$(' . join('|', @$NOT_FILTER) . ')(\,|\ \,|\,\ ).*?\);';
    $NOT_FILTER_REGEX = '\$[_a-zA-Z]+[_a-zA-Z0-9]*->write\(.*?\$(' . join('|', @$NOT_FILTER) . ')(\,|\ \,|\,\ ).*?\);';
    print STDERR __PACKAGE__, ":", __LINE__, ": ", "NOT_FILTER_REGEX     : $NOT_FILTER_REGEX\n"  if $ENV{LOG_FILTER_DEBUG};
  }

  unless ( defined $FILTER_ALL_REGEX ) {
    my $FILTER = [ @DEFAULT_FILTER ];
    $FILTER_ALL_REGEX = '(\$[_a-zA-Z]*[_a-zA-Z0-9]*log->write\(.*?)(' . join('|', @$FILTER) . ')(\,|\ \,|\,\ )(.*?\);)';
    print STDERR __PACKAGE__, ":", __LINE__, ": ", "FILTER_ALL_REGEX: $FILTER_ALL_REGEX\n"  if $ENV{LOG_FILTER_DEBUG};
  }

  unless ( defined $MATCH_LOG_LEVEL_REGEX ) {
    my $FILTER = [ LOG_LEVELS() ];
    $MATCH_LOG_LEVEL_REGEX = '(\$[_a-zA-Z]*[_a-zA-Z0-9]*log->write\(.*?)(' . join('|', @$FILTER) . ')(\,|\ \,|\,\ )(.*?\);)';
    print STDERR __PACKAGE__, ":", __LINE__, ": ", "MATCH_LOG_LEVEL_REGEX: $MATCH_LOG_LEVEL_REGEX\n"  if $ENV{LOG_FILTER_DEBUG};
  }
}

use Filter::Simple;
our $replace = '1;';
FILTER { # this filters out unwanted log messages from source code BEFORE COMPILATION
  # proves to be a great boon to performance
  $FILTER_CNT++;
  ##print STDERR __LINE__, ": \$ENV{LOG_FILTER} = $ENV{LOG_FILTER}\n";
  return if ( $ENV{LOG_FILTER} and $ENV{LOG_FILTER} =~ /^(OFF|)$/i);
  #return if ( $before =~ /\s*/s );
  my @caller = caller(1);
  $ENV{LOG_FILTER_DEBUG} ||= 0;
  print STDERR __PACKAGE__, ":", __LINE__, ": ", "CALLER: \n\t", join("\t\n", map { (defined $_ ? $_ : '')} @caller ), "\n" if ($ENV{LOG_FILTER_DEBUG} > 6);
  my $package = $caller[0];
  my $file    = $caller[1];
  my $calline = $caller[2];
  #print STDERR "." if $ENV{LOG_FILTER_DEBUG};
  my $debug_this_package = $ENV{LOG_FILTER_PACKAGES_DEBUG} ? $file =~ /$ENV{LOG_FILTER_PACKAGES_DEBUG}/ : 1;
  print STDERR __PACKAGE__, ":", __LINE__, ": ", "DEBUG_THIS_PACKAGE=$debug_this_package ... CALLED FROM FILE: '$file' ($ENV{LOG_FILTER_PACKAGES_DEBUG})\n" if $ENV{LOG_FILTER_DEBUG};
  my $not_filtered = $ENV{LOG_FILTER_DEBUG} ? "' ### LOG MESSAGE UN-FILTERABLE ### '" : '';
  my @match;
  my @before = split("\n", $_ );
  my @after = ();
  my $linenum = $calline;
  my $totallines = scalar @before;
  my $filtered = '';
  my $filtered_status = '';
  foreach my $line ( @before ) {
    $linenum++;
    $filtered = '';
    $filtered_status = '';
    if ( $line =~ /$MATCH_LOG_LEVEL_REGEX/ ) {
      $filtered_status = 'UNTOUCHED';
    } elsif ( $line =~ s/($FILTER_REGEX)\s*$/$replace/g ) {
      $filtered_status = 'FILTERED ';
      $filtered   = $1;
    } elsif ( $line =~ /$NOT_FILTER_REGEX/g ) {
      $filtered_status = 'NOT-FILTERED';
    } elsif ( $line =~ /$FILTER_ALL_REGEX/ ) { #and $line !~ /$not_filtered/ ) {
      print STDERR __PACKAGE__, ":", __LINE__, ": ",  "WARNING DEBUG LOG MESSAGE NOT REMOVED: $file : $linenum: $line \n" if ($ENV{WARN_FILTER} or ( $debug_this_package and $ENV{LOG_FILTER_DEBUG}));
      $line =~ s/$FILTER_ALL_REGEX/${1}${2},${not_filtered}${3}${4}/;
      $filtered_status = 'CHANGED';
    }
    push @after, $line;
    print STDERR __PACKAGE__, ":", __LINE__, ": ", pad(++$ENV{GLOBAL_LINES_FILTER_EXAMINED},5), ' |', pad($linenum,5), '/', space($totallines,5),': ', space($filtered_status, undef, '.'), '| ', $line, "\n" if ($debug_this_package and $ENV{LOG_FILTER_DEBUG} > 3);
    print STDERR __PACKAGE__, ":", __LINE__, ": ", pad('',5, 'x'), 'xx', pad('',5,'x'), 'x', space('',5,'x'),'::::: ', "FORMER CONTENTS: $filtered", "\n" if ( $debug_this_package and $ENV{LOG_FILTER_DEBUG} > 3 and $filtered );
  }
  $_ = join( "\n", @after) . "\n";
};

1;
