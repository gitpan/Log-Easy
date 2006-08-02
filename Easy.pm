package Log::Easy;
#  -t STDOUT -t STDERR ???
my $prefix_dev_backstack = 2;
use Log::Easy::Filter;
my $filter_file;
our $this_package;
BEGIN {
    # this little but of cruft really sucks, but neither 'require' nor 'do' are bahaving as I would expect(akin to a c #include)
    #require '/home/lengthe/cvs/adg/util/general/Log/Filter.pm';
    #require Log::Easy::Filter;
    #do Log::Easy::Filter;
    $filter_file = __PACKAGE__ eq 'Log::Easy' ? __FILE__ : ( $INC{'Log/Easy.pm'} or die "Couldn't find location of Log/Easy.pm package" );
    $filter_file =~ s|Easy.pm|Easy/Filter.pm|;
    print STDERR "filter_file=$filter_file\n"  if $ENV{LOG_FILTER_DEBUG};
    my $eval = 'package '  . __PACKAGE__ . ';';
    # this is somewhat evil, but I need to do it to get filtering in THIS package, as well as packages that use this package
    open(FILTER, "<$filter_file") or die $!;
    $eval .= '#' . join("", <FILTER>); #`cat $filter_file`; # the '#' here comments out the first line of the filter package 'package Log::Easy::Filter;'
    close FILTER;
    $eval =~ /(.*)/ms; # for untainting in case taint mode is on
    $eval = $1;
    print STDERR "EVAL:#########################\n$eval\n########################\n" if $ENV{LOG_FILTER_DEBUG};
    eval "{ $eval }";
    (print STDERR '$this_package: ', $this_package, '(', __PACKAGE__, ')', "\n") if $ENV{LOG_FILTER_DEBUG};
    $@ and die $@;
    #die;
}

#
use strict;
use Data::Dumper;
use IO::File;
use Fcntl qw(:flock);
use Carp qw( cluck confess );
use File::Spec;

if ( $ENV{LOG_USE_CARP} and $ENV{LOG_USE_CARP} eq 'YES'  ) {
    # big ugly stack traces when we encounter a 'warn' or a 'die'
    $SIG{__WARN__} = \&cluck;
    $SIG{__DIE__}  = \&confess;
}

use Getopt::Long;

use Exporter;
our ( %EXPORT_TAGS, @ISA, @EXPORT_OK, @EXPORT, $VERSION );
@ISA = qw( Exporter );

$VERSION = '0.01_11';

%EXPORT_TAGS = (
                # available constants for log level text names, these will never be filtered nor will warnings about them ever be made
                # basically, these are for production level logging (as opposed to the 'shorthand' log levels below in "log_level_[not_]filtered"
                # as such they can still be used to put the program in DEBUG mode (etc), but for more formalized debugging
                #log_level              => [ Log::Easy::Filter->LOG_LEVELS() ],
                log_level       => [ LOG_LEVELS() ],
                # global logging object
                log             => [ qw( $log log )   ],
                # convenient log level aliases that WILL BE FILTERED if appropriate (MUST begin with a $ [eg regular SCALAR variable]
                #log_level_filtered     => [ map { "\$$_" }  Log::Easy::Filter->DEFAULT_FILTER() ],
                ll_filtered     => [ map { "\$$_" }  DEFAULT_FILTER() ],
                # same as above, but without '$', these will not be filtered, but if $ENV{WARN_FILTER} is set, warnings about unfiltered log messages will show up
                # this is useful for debugging when you may want a particular message to be displayed (simply delete the '$')
                #log_level_not_filtered => [                 Log::Easy::Filter->DEFAULT_FILTER() ],
                ll_not_filtered => [                 DEFAULT_FILTER() ],
                # these are utility methods for output formatting
                misc            => [ qw(space pad dump _caller $hostname ) ],
                usage           => [ qw( get_options optargs_missing usage) ],
               );

$EXPORT_TAGS{all}        = [ map {@{$_}} values %EXPORT_TAGS ];
$EXPORT_TAGS{initialize} = [ @{$EXPORT_TAGS{log_level}} ];
$EXPORT_TAGS{basic}      = [ map { @{$EXPORT_TAGS{$_}} } qw( log_level log ll_filtered ll_not_filtered) ];
@EXPORT_OK = @{$EXPORT_TAGS{'all'}};
@EXPORT = ();

use constant MESSAGE => 'MESSAGE'; # this will send an email to the appointed person
use constant DEFAULT => 'DEFAULT';
use constant LOUD    => 'LOUD';
use constant CLEAN   => 'CLEAN';
use constant EMERG   => 'EMERG';
use constant ALERT   => 'ALERT';
use constant QUIT    => 'QUIT';
use constant EXIT    => 'QUIT'; # synonym for QUIT
use constant CRIT    => 'CRIT';
use constant FATAL   => 'FATAL'; # synonym for CRIT
use constant FAIL    => 'FAIL'; # synonym for CRIT
use constant ERROR   => 'ERROR';
use constant WARN    => 'WARN';
use constant NOTICE  => 'NOTICE';
use constant INFO    => 'INFO';
use constant DEBUG99 => 'DEBUG99';
use constant DEBUG9  => 'DEBUG9';
use constant DEBUG8  => 'DEBUG8';
use constant DEBUG7  => 'DEBUG7';
use constant DEBUG6  => 'DEBUG6';
use constant DEBUG5  => 'DEBUG5';
use constant DEBUG4  => 'DEBUG4';
use constant DEBUG3  => 'DEBUG3';
use constant DEBUG2  => 'DEBUG2';
use constant DEBUG1  => 'DEBUG1';
use constant DEBUG0  => 'DEBUG0';
use constant DEBUG   => 'DEBUG';
use constant TRACE   => 'TRACE';
use constant SPEW    => 'SPEW';

use constant D_MESSAGE => 'D_MESSAGE'; # this will send an email to the appointed person
use constant D_DEFAULT => 'D_DEFAULT';
use constant D_LOUD    => 'D_LOUD';
use constant D_CLEAN   => 'D_CLEAN';
use constant D_EMERG   => 'D_EMERG';
use constant D_ALERT   => 'D_ALERT';
use constant D_CRIT    => 'D_CRIT';
use constant D_FATAL   => 'D_FATAL';
use constant D_FAIL    => 'D_FAIL';
use constant D_QUIT    => 'D_QUIT';
use constant D_EXIT    => 'D_EXIT';
use constant D_ERROR   => 'D_ERROR';
use constant D_WARN    => 'D_WARN';
use constant D_NOTICE  => 'D_NOTICE';
use constant D_INFO    => 'D_INFO';
use constant D_DEBUG99 => 'D_DEBUG99';
use constant D_DEBUG9  => 'D_DEBUG9';
use constant D_DEBUG8  => 'D_DEBUG8';
use constant D_DEBUG7  => 'D_DEBUG7';
use constant D_DEBUG6  => 'D_DEBUG6';
use constant D_DEBUG5  => 'D_DEBUG5';
use constant D_DEBUG4  => 'D_DEBUG4';
use constant D_DEBUG3  => 'D_DEBUG3';
use constant D_DEBUG2  => 'D_DEBUG2';
use constant D_DEBUG1  => 'D_DEBUG1';
use constant D_DEBUG0  => 'D_DEBUG0';
use constant D_DEBUG   => 'D_DEBUG';
use constant D_TRACE   => 'D_TRACE';
use constant D_SPEW    => 'D_SPEW';



# the following, when used as log levels in code calling this package with qw(:all)
# these may not be worth the clutter
# I have also made identically named scalars which if used will  cause the log messages to be filtered out
# WARNING: without the `$' the log message WILL NOT be filtered out!
use constant  ll   => D_DEFAULT;
use constant  mll  => D_MESSAGE;
use constant  lll  => D_LOUD;
use constant  cll  => D_CLEAN;
use constant  qll  => D_QUIT;
use constant  ell  => D_ERROR;
use constant  all  => D_ALERT;
use constant  wll  => D_WARN;
use constant  nll  => D_NOTICE;
use constant  ill  => D_INFO;
use constant  dl99 => D_DEBUG99;
use constant  dl9  => D_DEBUG9;
use constant  dl8  => D_DEBUG8;
use constant  dl7  => D_DEBUG7;
use constant  dl6  => D_DEBUG6;
use constant  dl5  => D_DEBUG5;
use constant  dl4  => D_DEBUG4;
use constant  dl3  => D_DEBUG3;
use constant  dl2  => D_DEBUG2;
use constant  dl1  => D_DEBUG1;
use constant  dl0  => D_DEBUG0;
use constant  dll  => D_DEBUG;
use constant  tll  => D_TRACE;
use constant  sll  => D_SPEW;


our ( $p_space, $p_pad ) = ( 8, 8 );
our $STACK_TRACE = $ENV{LOG_STACK_TRACE} || 0;

our ( $DUMPER, $log_level, $log, $intlog );

# if we have big warngings set to true for any particular log level then we'll issue a perl 'warn'ing
our %BIG_WARN_DEFAULTS = ( ( map { ("DEBUG$_" => 0); } ( 0 .. 9 ) ),
                           ( map { ($_ => 0);} qw( MESSAGE LOUD CLEAN QUIT EXIT EMERG ALERT CRIT FATAL FAIL ERROR WARN NOTICE INFO DEBUG TRACE SPEW ) ),
                           ( qw( WARN 0 ERROR 0 CRIT 1 FATAL 1 FAIL 0 ) )
                         );
#our %BIG_WARN_ON       = map { print STDERR qq'BIG_WARN_ON_$_ => ', ( defined $ENV{"BIG_WARN_ON_$_"} ? $ENV{"BIG_WARN_ON_$_"} : 'undef' ), "\n"; ( $_ => ( defined $ENV{"BIG_WARN_ON_$_"} ? $ENV{"BIG_WARN_ON_$_"} : ( $BIG_WARN_DEFAULTS{$_} || 0 ) )); }  keys %BIG_WARN_DEFAULTS;
our %BIG_WARN_ON       = map { ( $_ => ( defined $ENV{"BIG_WARN_ON_$_"} ? $ENV{"BIG_WARN_ON_$_"} : ( $BIG_WARN_DEFAULTS{$_} || 0 ) )); }  keys %BIG_WARN_DEFAULTS;
# these were(are?) actually apache constants for logging levels I think anything that gets in that
# is preceded with a '_' gets [0] (numerical value) these return the uppercase(?) version of
# themselves
our %LOG_CODE = ( STDERR  => 0x00E0,
                  STDOUT  => 0x00E0,
                  CLEAN   => 0x00E0,
                  MESSAGE => 0x00E0,
                  LOUD    => 0x00E0,
                  CRIT    => 0x00E0,
                  FATAL   => 0x00E0,
                  FAIL    => 0x00E0,
                  QUIT    => 0x00E0,
                  EXIT    => 0x00E0,# synonym for QUIT
                  EMERG   => 0x00E0,
                  ALERT   => 0x0080,
                  ERROR   => 0x0070,
                  WARN    => 0x0060,
                  NOTICE  => 0x0050,
                  INFO    => 0x0040,
                  DEBUG99    => 0x0040, # this is the same as INFO, but will cause the line number and package to be printed with EVERY log call if LOG_LEVEL is set to anything that matched '.*DEBUG.*' 
                  (map { ("DEBUG$_" => ( 0x0030 + $_ )); } ( 0 .. 9 )),
                  DEBUG   => 0x0030,
                  TRACE   => 0x0020,
                  SPEW    => 0x0010,
                  DEFAULT => 0x0030,# set equal to DEBUG
                );
# translate between our more expanded selection of logging levels to what apache understands
our %APACHE_LEVELS = ( DEFAULT => INFO,
                       TRACE   => DEBUG,
                       SPEW    => DEBUG,
                       DEBUG   => DEBUG,
                       (map { ("DEBUG$_" => 'DEBUG'); } ( 0 .. 9, 99 )),
                       INFO    => INFO,
                       WARN    => WARN,
                       NOTICE  => NOTICE,
                       CRIT    => CRIT,
                       FATAL   => CRIT,
                       FAIL    => CRIT,
                       QUIT    => CRIT,
                       EXIT    => CRIT,
                       ERROR   => ERROR,
                       ALERT   => ALERT,
                       EMERG   => EMERG,
                       LOUD    => ERROR,
                       CLEAN   => ERROR,
                     );

our ( $ll, $lll, $qll, $cll, $ell, $all, $wll, $nll, $ill, $dll, $tll, $sll, $mll, $dl0, $dl1, $dl2, $dl3, $dl4, $dl5, $dl6, $dl7, $dl8, $dl9, $dl99 )
  = (  ll,  lll,  qll,  cll,  ell,  all,  wll,  nll,  ill,  dll,  tll,  sll,  mll,  dl0,  dl1,  dl2,  dl3,  dl4,  dl5,  dl6,  dl7,  dl8,  dl9, dl99 );
our $n;
our %LEVEL_FHS = map { ($_ => 'STDERR'); } qw(EMERG ALERT CRIT FATAL FAIL ERROR WARN QUIT);

#%ALWAYS_LOG is for log levels that should never be dropped, even if the package is blocked from logging
our %ALWAYS_LOG = qw(
                     CLEAN   1
                     CRIT    1
                     FATAL   1
                     FAIL    1
                     QUIT    1
                     ERROR   1
                     ALERT   1
                     EMERG   1
                     MESSAGE 1
                    );
our $default_log_level = 'INFO';
$log_level = $ENV{LOG_LEVEL} ||= ( [ map {$ENV{$_}?$_:()}(@{$EXPORT_TAGS{log_level}}) ]->[0] || $default_log_level );
# message terminator (sometimes we DON'T want newlines!)

our $default_handle_fatals = 1;
our $default_unbuffer = 1;
our $default_fh = $ENV{LOG_FILE_DEFAULT} || $ENV{DEFAULT_LOG_FILE} || 'STDOUT';
our %init = ( log_file       => $ENV{LOG_FILE} || $default_fh ,
              log_level      => $log_level,
              dump_refs      => (exists $ENV{LOG_DUMP_REFS}    ) ? $ENV{LOG_DUMP_REFS}     : 1 ,
              handle_fatals  => (exists $ENV{LOG_HANDLE_FATALS}) ? $ENV{LOG_HANDLE_FATALS} : $default_handle_fatals,
              exclusive      => $ENV{LOG_EXCLUSIVE} || '',
              unbuffer       => (exists $ENV{LOG_UNBUFFER} ? $ENV{LOG_UNBUFFER} : $default_unbuffer),
              #prefix         => \&_prefix_default,
            );

our %FHS_NO = (); # store list of filehandles indexed by fileno()
our %FHS_NA = (); # store list of filehandles indexed by file name
our %FHN_NO = (); # corresponding list of filenames for our filehandles indexed by fileno()
# OK .. I'm not sure, but trying to use STDIN may be totally retarded
#@LEVEL_FHS{qw( STDIN  STDOUT STDERR )} = ( \*STDIN , \*STDOUT, \*STDERR );
@FHS_NA{qw( STDIN  STDOUT STDERR )} = ( \*STDIN , \*STDOUT, \*STDERR );
@FHN_NO{(map { fileno($_); } @FHS_NA{qw( STDIN  STDOUT STDERR )})} = qw( STDIN  STDOUT STDERR );
@FHS_NO{keys %FHN_NO} = values %FHN_NO;
foreach my $fh ( @FHS_NA{qw( STDOUT STDERR )} ) { $log->{unbuffer} ? _unbuffer( $fh ) : (); }


$log = $this_package->new();
$intlog = $this_package->new( { prefix => \&_prefix_dev } );

our $hostname = `hostname`;
#print STDERR '$hostname: ', $hostname;
chomp $hostname;
$intlog->write($dll, '$hostname: ', $hostname );

our @userinfo = getpwuid $<;
our $username = $userinfo[0];

my @pathinfo = (File::Spec->splitpath( File::Spec->rel2abs( $0 )));
$intlog->write({prefix=>undef},$sll, '@pathinfo: ', \@pathinfo );

my $path_base = $0;
my @o = split( m|/|, $path_base );
$intlog->write($dll, '@o: ', \@o );
my $max_path_seg = 3;
my $num_path_seg  = scalar @o;
#my $path_abbrev = ( $num_path_seg > $max_path_seg ) ? join('/', map {''} ( 1 .. ( $num_path_seg - $max_path_seg ))), '...', @o[$#o - 1 .. $#o ] ) : $path_base;
#my $path_abbrev = ( $num_path_seg > $max_path_seg ) ? join('/', (@o[0 .. 2], map {''} ( 4 .. ( $num_path_seg - $max_path_seg ))), '...', @o[$#o - 1 .. $#o ] ) : $path_base;
my $path_abbrev = ( $num_path_seg > $max_path_seg ) ? join('/', @o[0 .. 2], '...', @o[$#o - 1 .. $#o ] ) : $path_base;

#my $xxx = $intlog;
#$xxx->write('STDERR', '%ENV{BIG_WARN_ON_XXX}: ', { map { $_ => ( $ENV{"BIG_WARN_ON_$_"} || 0 ) } keys%BIG_WARN_DEFAULTS } );
#$xxx->write('STDERR', '%BIG_WARN_DEFAULTS: ', \%BIG_WARN_DEFAULTS );
#$xxx->write('STDERR', '%BIG_WARN_ON: ', \%BIG_WARN_ON );

# we don't normally want a stack trace on every log call
# enable on any particular call with: $intlog->write({st=>1},$lll, ':');
# enable on all calls with: $log->stack_trace( 1 );


*always_log = \*ALWAYS_LOG;
sub ALWAYS_LOG {
    my $self = shift;
    my $log_level = shift;
    $log_level or return %ALWAYS_LOG;
    $log_level =~ s/^D_//;
    return $ALWAYS_LOG{$log_level};
}


#$intlog->write($lll, '%LOG_CODE: ', "\n", map { (space($_->[0]), ' => ', pad( $_->[1]), "\n") } sort { $a->[1] <=> $b->[1]; } map { [ $_ => $LOG_CODE{$_} ]; } keys %LOG_CODE );
#$intlog->packages('!' . $this_package); # uncomment this to disable all internal logging

$ENV{LOG_PACKAGES} ||= '';
if ( $ENV{LOG_PACKAGES} ) {
    $log->packages($ENV{LOG_PACKAGES});
    $intlog->packages($ENV{LOG_PACKAGES});
}


# the following two sets of exported variables/subs are for development debugging purposes and are
# filtered out at compile time, unless $ENV{LOG_FILTER} is appropriately set. I'm thinking that since
# these are for development debugging that they should maybe have some different significance when
# it comes to descriptive output. Currently all log messages output the &{$log->{prefix}}(). Perhaps
# we should use a bitmask to determine whether or not a log should be output and additionally what
# kind of prefix it has. This would allow these to mimic the "production" log levels (in value)
# while also allowing us to have more descriptive prefix (caller, etc...) when they are used for
# development debugging

*log_code = \*LOG_CODE;
sub LOG_CODE {
    my $self = shift;
    my $log_level = shift;
    $log_level or return %LOG_CODE;
    $log_level =~ s/^D_//;
    return $LOG_CODE{$log_level};
}

sub n              { exists $_[1] ? $_[0]->{ n              } = $_[1] : $_[0]->{ n             }; }
sub log            { exists $_[1] ? $_[0]->{ log            } = $_[1] : $_[0]->{ log           }; }
#sub log {
#    if ( $_[0] and UNIVERSAL::isa( $_[0], __PACKAGE__ ) ) {
#        return exists $_[1] ? $_[0]->{ log            } = $_[1] : $_[0]->{ log           };
#    } else {
#        return $log;
#    }
#}
sub log_level      { exists $_[1] ? $_[0]->{ log_level      } = $_[1] : $_[0]->{ log_level     }; }
sub dump_refs      { exists $_[1] ? $_[0]->{ dump_refs      } = $_[1] : $_[0]->{ dump_refs     }; }
sub handle_fatals  { exists $_[1] ? $_[0]->{ handle_fatals  } = $_[1] : $_[0]->{ handle_fatals }; }
sub exclusive      { exists $_[1] ? $_[0]->{ exclusive      } = $_[1] : $_[0]->{ exclusive     }; }
sub stack_trace    { exists $_[1] ? $_[0]->{ stack_trace    } = $_[1] : $_[0]->{ stack_trace   }; }
sub email          { exists $_[1] ? $_[0]->{ email          } = $_[1] : $_[0]->{ email         }; }
sub prefix         { exists $_[1] ? $_[0]->{ prefix         } = $_[1] : $_[0]->{ prefix        }; }
sub terse          { exists $_[1] ? $_[0]->{ terse          } = $_[1] : $_[0]->{ terse         }; }
sub unbuffer       { exists $_[1] ? $_[0]->{ unbuffer       } = $_[1] : $_[0]->{ unbuffer      }; }
*autoflush = \&unbuffer;
sub log_file       {
    # this needs to be able to take a file handle as well as a filename or symbolic filehandle name (eg 'STDOUT')
    # I was going to set up something here to be able to pass in a whole list of LEVEL => $file pairs, but on second though, just call the method repeatedly
    my $self  = shift;
    my $level = shift || '';
    #my @caller = _caller();
    #  print STDERR __LINE__, "-" x 80, "\n", @caller, "\n";#  if $ENV{LOG_PACKAGES_DEBUG};
    #  print STDERR __LINE__, " LEVEL=$level\n";#  if $ENV{LOG_PACKAGES_DEBUG};
    my $dest  = shift || '';
    #  print STDERR __LINE__, " DEST=$dest\n";#  if $ENV{LOG_PACKAGES_DEBUG};
    my $key = 'log_file';
    #  print STDERR __LINE__, " KEY=$key\n";#  if $ENV{LOG_PACKAGES_DEBUG};
    my $valid_level  = scalar map { $_ eq $level ? 1 : (); } LOG_LEVELS() if $level;
    if ( $level and not $valid_level ) {
        if ( $dest ) {
            #houston we have a problem
        } else {
            $dest = $level;
        }
    } elsif ( $level and $valid_level ) {
        $key .= "_$level";
    }
    
    if ( $dest ) {
        $self->{$key} = $dest;
    }
    #  print STDERR __LINE__, " VALID_LEVEL=$valid_level\n";#  if $ENV{LOG_PACKAGES_DEBUG};
    #  print STDERR __LINE__, " LEVEL=$level\n";#  if $ENV{LOG_PACKAGES_DEBUG};
    #  print STDERR __LINE__, " DEST=$dest\n";#  if $ENV{LOG_PACKAGES_DEBUG};
    #  print STDERR __LINE__, " KEY=$key\n";#  if $ENV{LOG_PACKAGES_DEBUG};
    #  print STDERR __LINE__, " RETURN=$self->{$key}\n";#  if $ENV{LOG_PACKAGES_DEBUG};
    #  print STDERR __LINE__, "-" x 80, "\n";#  if $ENV{LOG_PACKAGES_DEBUG};
    return $self->{$key};
}
sub log_file_multiplex {
    my $self  = shift;
    # I should change this to accept filehandles as well
    if ( scalar @_ > 2 ) {
        die "
Called with too many arguments
the several ways this could be called, maximum of 2 arguments allowed
0: ()                                                           ===> with no arguments, return the log_file unadorned with a specific log_level
1: <FILE>                                                       ===> set the log_file for any LEVEL not otherwise spoken for to the specified FILE (or 'STDERR', 'STDOUT')
2: <LEVEL>                                                      ===> return the log_file for the LEVEL specified
3: [ <FILE1>, <FILE2, ..., <FILEn> ]                            ===> set the log_file for any LEVEL not otherwise spoken for to be multiplexed across the specified files in list  [ FILE1, FILE2, ..., FILEn]
4: [ <LEVEL1>, <LEVEL2>, ..., <LEVELn> ]                        ===> return the log_file for the list of LEVELs specified
5: <LEVEL> => <FILE>                                            ===> set the log_file for the LEVEL specified to FILE
6: <LEVEL> => [ <FILE1>, <FILE2>, ..., <FILEn> ]                ===> set the log_file for the LEVEL specified to multiplex across files in list [ FILE1, FILE2, ..., FILEn]
7: [ <LEVEL1>, <LEVEL2> ] => <FILE>                             ===> set the log_file for the LEVELS specified to the same file FILE
8: [ <LEVEL1>, <LEVEL2> ] => [ <FILE1>, <FILE2>, ..., <FILEn> ] ===> set the log_file for the LEVELS specified to multiplex across files in list [ FILE1, FILE2, ..., FILEn]
";
    }
    my $key = 'log_file';
    #$key = 'log_file_multiplex';
    
    my $level = shift || '';
    my $dest  = shift || '';
    if ( not $level and not $dest ) {
        ######
        return $self->{$key};
        ######
        ######################################
    }
    
    my $reflevel;
    my $refdest;
    unless ( ref $level eq 'ARRAY' ) {
        $reflevel = 0;
        $level = [ $level ];
    } else {
        $reflevel = 1;
    }
    
    if ( $level and not $dest ) {
        # check to see if this is specifying just a level, or just a dest
        my $valid_level  = scalar map { $_ eq $level->[0] ? 1 : (); } LOG_LEVELS() if $level->[0];
        if ( $valid_level ) {
            my @return;
            foreach my $l ( @$level ) {
                my $vl  = scalar map { $_ eq $l ? 1 : (); } LOG_LEVELS();
                unless ( $vl ) {
                    die "
Something is awry with the arguments you passed:
" . join(', ', @$level ) . "
";
                } else {
                    push @return, $self->{$key}{$l};
                }
            }
            ######
            return $reflevel ? \@return : $return[0];
            ######
            ######################################
        } else {
            # if the arg is not a valid log level then it must be a destination file or filehandle
            $refdest = $reflevel;
            $dest = $level;
            undef $level;
        }
    }
    
    unless ( ref $dest eq 'ARRAY' ) {
        $refdest = 0;
        $dest = [ $dest ];
    } else {
        $refdest = defined $refdest ? $refdest : 0;
    }
    
    if ( $dest and not $level ) {
        # we got only one argument and it was a destination without the level specified
        # this means by default we want to multiplex across the files given
        $self->{$key} = $dest;
        ######
        return $self->{$key};
        ######
        ######################################
    }
    
    # here we have both level and dest, which should each now be array refs
    foreach my $l ( @$level ) {
        my $k = "${key}_$l";
        my $pd = $self->{$k};
        # check to see where $pd and $dest do not agree, close all filehandles in$pd which are not also in $dest
        $self->{$k} = $dest;
    }
    return $self->{$key};
}

sub packages {
    # this sets up lists of DO and DONT log for packages specified at runtime
    # if any DO log lists are set up then we will log ONLY from packages who appear in the DO list EVEN IF they are also in the DONT list
    # if any DONT log lists are set up then we will NEVER log from packages in the DONT log list UNLESS they are in the DO log list
    my $self = shift;
    if ( exists $_[0] ) {
        my @new_packages = @_;
        my $packages = $self->{packages_array} ||= [];
        my $do_log   = $packages->[0] ||= [];
        my $dont_log = $packages->[1] ||= [];
        foreach my $package_set ( @new_packages ) {
            my @package_set = split(/\#/, $package_set );
            foreach my $package ( @package_set ) {
                next unless ($package and $package !~ /^\s+$/);
                print STDERR __PACKAGE__, ":", __LINE__, ": ", '$package: '  , $package, "\n"  if $ENV{LOG_PACKAGES_DEBUG};
                if ( $package =~ s/^\!// ) {
                    #it's a dont
                    unless( grep { /^$package$/ } @$dont_log ) {
                        print STDERR __PACKAGE__, ":", __LINE__, ": ", 'DONT ::: $package: \''  , $package, "'\n"  if $ENV{LOG_PACKAGES_DEBUG};
                        push @$dont_log, $package;
                    }
                } else {
                    unless( grep { /^$package$/ } @$do_log ) {
                        print STDERR __PACKAGE__, ":", __LINE__, ": ", 'DO ::: $package: \''  , $package, "'\n"  if $ENV{LOG_PACKAGES_DEBUG};
                        push @$do_log, $package;
                    }
                }
            }
        }
        if ( my $packages = $self->{packages_array} ) {
            my $do_log   = $packages->[0];
            print STDERR __PACKAGE__, ":", __LINE__, ": ", '$do_log: '  , scalar @$do_log  , " :: '", join('|', @$do_log)  , "'\n"  if $ENV{LOG_PACKAGES_DEBUG};
            my $dont_log = $packages->[1];
            print STDERR __PACKAGE__, ":", __LINE__, ": ", '$dont_log: ', scalar @$dont_log, " :: '", join('|', @$dont_log), "'\n"  if $ENV{LOG_PACKAGES_DEBUG};
            my $packages_rx = $self->{packages} ||= [];
            my $do_log_rx   = scalar @$do_log   ? [ map { qr/$_/; } @$do_log   ] : []; #scalar @$do_log   ? join('|', @$do_log )   : undef;
            print STDERR __PACKAGE__, ":", __LINE__, ": ", '$do_log_rx: '  , scalar @$do_log_rx  , " :: '", join('|', @$do_log_rx)  , "'\n"  if $ENV{LOG_PACKAGES_DEBUG};
            my $dont_log_rx = scalar @$dont_log ? [ map { qr/$_/; } @$dont_log ] : []; #scalar @$dont_log ? join('|', @$dont_log ) : undef;
            print STDERR __PACKAGE__, ":", __LINE__, ": ", '$dont_log_rx: ', scalar @$dont_log_rx, " :: '", join('|', @$dont_log_rx), "'\n"  if $ENV{LOG_PACKAGES_DEBUG};
            $packages_rx->[0] = $do_log_rx;
            $packages_rx->[1] = $dont_log_rx;
        }
    }
    return $self->{packages};
}

sub clone {
    my $self = shift;
    my $VAR1 = $self->dump( @_ );
    my $clone = eval $VAR1;
    $clone->{prefix} = $self->{prefix} if ( UNIVERSAL::isa( $clone, $this_package ) and ref $self->{prefix} eq 'CODE' );
    return $clone;
}



#print STDERR $this_package, " STDERR ", __LINE__, " ::: OH MY! ... ", $log->dump([ \@_ ]);
#print STDOUT $this_package, " STDOUT ", __LINE__, " ::: OH MY! ... ", $log->dump([ \@_ ]);
sub new {
    #print STDERR _caller();
    my $self = shift;
    my $class = ref $self || $self || $this_package;
    $self = bless {}, $class;
    $self->init( @_ );
    return $self;
}

sub init {
    my $self = shift;
    my $init = shift;
    if ( defined $init ) {
        unless ( ref $init eq 'HASH' ) {
            unshift @_, $init;
            $init = { @_ };
        }
    } else {
        $init = {};
    }
    $init = { %init , %$init }; # override defaults with init args passed in
    while ( my ( $key, $value ) = each %$init ) {
        next unless $key;
        #$self->{$key} = $value;
        $self->$key( $value );
    }
    while ( my ( $level, $fh ) = each %LEVEL_FHS ) {
        $self->log_file( $level => $fh );
    }
    #print STDERR "$self: ", &dump( $self, -d => $self );
    return $self;
};

sub dump {
    my $DUMP = '';
    my $self = shift;
    (print STDERR $this_package, " STDERR ", __LINE__, " ::: OH MY!:: ", __PACKAGE__->_caller(), Dumper(\@_), "\n") if $ENV{LOG_INTERNAL_DUMP_DEBUG};
    my $class = ref $self || $self;
    my ( $dumps, $names );
    my ( $pure, $deep,  $indent, $id, $terse, $pad );
    if ( $_[0] and $_[0] =~ /^-/ ) { 
        my $args = { @_ };
        $dumps    = $args->{-d} || $args->{-dump}    || $self;
        $names    = $args->{-n} || $args->{-names}   || undef;
        $dumps  = [ $dumps ] unless ( ref $names eq 'ARRAY' );
        $pure     =                $args->{-pure}    || 0    ;
        $deep     =                $args->{-deep}    || 0    ;
        $indent   = ((defined $args->{-i})? ($args->{-i}) : (defined $args->{-indent} ? $args->{-indent} : undef ));
        $id       =                $args->{-id}      || 0;
        $terse    =                $args->{-terse}   || 0    ;
        $pad      = $args->{-p} || $args->{-pad}     || '';
        if ( $terse and not defined $indent ) {
            $indent = 0;
        }
        if ( not defined $indent ) {
            $indent = 2;
        }
    } else {
        $dumps    = shift || $self;
        $names    = shift || undef;
        $pure     = shift || 0;
        $deep     = shift || 0;
        $indent   = shift || 2;
        $id       = shift || 0;
        $terse    = shift || 0;
        $pad      = shift || '';
    }
    (print STDERR $this_package, " STDERR ", __LINE__, " ::: OH MY!:: ", __PACKAGE__->_caller(), Dumper([( $pure, $deep,  $indent, $id, $terse)]), "\n") if $ENV{LOG_INTERNAL_DUMP_DEBUG};
    
    ( defined $dumps ) and ( ref $dumps eq 'ARRAY' ) or ( $dumps = [ $dumps ] );
    ( defined $names ) and ( ref $names eq 'ARRAY' ) or ( $names = [ $names ] );
    if ( $id ) {
        for( my $i = 0; $i <= $#$dumps; $i++ ) {
            my $d = $dumps->[$i];
            my $n = ref $d ? $d : \$d;
            $names->[$i] = $n;
        }
    }
    my $dumper = Data::Dumper->new( $dumps , $names );
    
    $dumper->Pad     ( $pad    ) if $pad;
    $dumper->Purity  ( $pure   ) if $pure;
    $dumper->Deepcopy( $deep   ) if $deep;
    $dumper->Terse   ( $terse  ) if $terse;
    $dumper->Indent  ( $indent ) if defined $indent;
    $DUMP = $dumper->Dump();
    return $DUMP
}

sub get_options {
    my $optargs_orig = shift;
    my $optargs = { %$optargs_orig };
    $intlog->write($dll, '$optargs: ', $optargs );
    $intlog->write($dll, '@_: ', \@_ );
    my $optconfig = ref $_[0] eq 'HASH' ? (shift) : {};
    $intlog->write($dll, '$optconfig: ', $optconfig );
    $intlog->write($dll, '@_: ', \@_ );
    my $config    = ref $_[0] eq 'HASH' ? (shift) : {};
    $intlog->write($dll, '$config: ', $config );

    my %options;
    my $target  = $config->{target}  || \%options;
    my $default = $config->{default} || $target;

    # give some easy usage/help options
    if ( not $config->{nohelp} or exists $config->{help} and $config->{help} ) {
        $optargs->{help}  ||= $optargs->{usage} ||= [''   , sub { usage( $optargs, \%options, $optconfig, $config ) }, 'print help message and exit'];
        $optargs->{usage} ||= $optargs->{help}  ||= [''   , sub { usage( $optargs, \%options, $optconfig, $config ) }, 'print help message and exit'];
    }
    my $no_usage    = $config->{no_usage}   || scalar (grep { /no_usage/   } @_) ? 1 : 0;
    $intlog->write($sll, '$no_usage: ', $no_usage );
    my $no_missing  = $config->{no_missing} || scalar (grep { /no_missing/ } @_) ? 1 : 0;
    $intlog->write($sll, '$no_missing: ', $no_missing );
    my $p = new Getopt::Long::Parser config => [ map { ($optconfig->{$_} ? $_ : ($_ =~ /^no_/ ? $_ : "no_$_")); } keys %$optconfig ];
    my %GetOptions = map {  my $optname = $_;
                            $intlog->write($dll, '$optname: ', $optname );
                            my $progopt = $optargs->{$_}[0];
                            $intlog->write($dll, '$progopt: ', $progopt );
                            my $argspec = '';
                            my $optspec  = '';
                            my $type;
                            if ( $progopt =~ /^([:=]+)([ifs@%])$/ ) {
                                my $spec = $1;
                                $type     = $2;
                                $argspec = substr($spec, 0 , 1);
                                $optspec  = substr($spec, 1 , 1) || $argspec;
                                $argspec .= $type;
                                $optspec  .= $type;
                            } else {
                                $argspec = $progopt;
                                $optspec  = $progopt;
                            }
                            #("$_$optargs->{$_}[0]" => ref $optargs->{$_}[1] ? $optargs->{$_}[1] : \$optargs->{$_}[1])
                            $intlog->write($dll, '$optspec: ', $optspec );
                            $intlog->write($dll, '$argspec: ', $argspec );
                            my @opt = ( "$optname$optspec" => ref $optargs->{$_}[1] ? $optargs->{$_}[1] : \$optargs->{$_}[1] );
                            $intlog->write($dll, '@opt: ', \@opt );
                            @opt;
                        } keys %$optargs;
    
    $intlog->write($dll, '%GetOptions: ', \%GetOptions);
    local $SIG{__WARN__} = sub { &failed_options(@_) };# may want to add some additional arguments to pass to failed_options
    $log->write($dll, '@ARGV: ', \@ARGV );
    my $opt = $p->getoptions( %GetOptions );
    $intlog->write($dll, '%GetOptions: ', \%GetOptions);
    $log->write($dll, '@ARGV: ', \@ARGV );
    $log->write($dll, '$opt: ', $opt );
    $intlog->write($dl7, '$optargs: ', $optargs );
    $intlog->write($dl7, '$opt: ', $opt);
    # check that all required options have been provided
    my @missing = $no_missing ? () : optargs_missing( $optargs );
    $intlog->write($sll, "\@missing: ($no_missing)", \@missing );
    #return () if (scalar @missing and $no_usage);

    # when this routine is separated into its own package (Getargs::Long ???), the %options should
    # probably be a tied hash with a canonical set of key names and a 'hidden' set of aliases as
    # specified by the $key of %optargs (eg my %o = ( 'foo|bar|baz' => [ '!', undef, 'some flag'] );
    # would have a canonical name 'foo' (returned by keys %o) and aliases 'bar' and 'baz'

    foreach my $key ( keys %$optargs ) {
        $intlog->write($dll, '$key: ', $key );
        my $value = defined $optargs->{$key}[1] ? $optargs->{$key}[1] : $default->{$key};
        $intlog->write($dll, '$value: ', $value );
        my @aliases = ();# = ( $key );
        if ( $key =~ /\|/ ) {
            push  @aliases, split /\|/, $key;
        } else {
            push  @aliases, $key;
        }
        $intlog->write($dll, '@aliases: ', \@aliases );
        foreach my $alias ( @aliases ) {
            if (  ref $value eq 'SCALAR' ) {
                $options{$alias} = $$value;
            } elsif ( ref $value eq 'CODE' ) {
                #$options{$alias} = $value;
            } elsif ( ref $value eq 'ARRAY' ) {
                my $final_value = [];
                foreach my $item ( @$value ) {
                    if ( ref $item eq 'ARRAY' ) {
                        push @$final_value, @$item;
                    }
                    else {
                        push @$final_value, $item;
                    }
                }
                $options{$alias} = $final_value;
            } else {
                $options{$alias} = $value;
            }
            $target->{$alias} = $options{$alias};
        }
    }
    return usage( $optargs, \%options, $optconfig, $config ) if (scalar @missing and not $no_usage);
    $intlog->write($dl7, '%options: ', \%options);
    return %options;
}

sub failed_options {
    $intlog->write({EXIT => 1, prefix => ''}, EXIT, @_);
}

sub optargs_missing {
    my $optargs = shift;
    $intlog->write($dl7, '$optargs: ', $optargs );
    my $options = shift || {};
    $intlog->write($dl7, '$options: ', $options );
    my @missing;
    my %options;
    my @aliases = ();
    my %aliases = ();
    foreach my $key ( keys %$optargs ) {
        next if $key =~ /^(usage|help)$/;
        $intlog->write($dl7, '$key: ', $key );
        my @a;
        my $value = $optargs->{$key}[1];
        ref $value eq 'SCALAR' and $value = $$value;
        $intlog->write($dl7, '$value: ', $value );
        if ( $key =~ /\|/ ) {
            push  @a, split /\|/, $key;
        } else {
            push  @a, $key;
        }
        push  @aliases, @a;
        $aliases{$key} = \@a;
        foreach my $alias ( @a ) {
            $intlog->write($dl7, '$alias: ', $alias );
            $options{$alias} = defined $options->{$alias} ? $options->{$alias} : $value;
            $intlog->write($dl7, qq'\$options{$alias}: ', $options{$alias} );
        }
    }
    $intlog->write($dl7, '%options: ', \%options );
    foreach my $opt (keys %$optargs ) {
        $intlog->write($dl7, '$opt: ', $opt );
        my $optspec = $optargs->{$opt}[0];
        $intlog->write($dl7, '$optspec: ', $optspec );
        next if not $optspec;
        my $a     = $aliases{$opt};
        $intlog->write($dl7, '$a: ', $a );
        my $alias = $a->[0];
        $intlog->write($dl7, '$alias: ', $alias );
        my $value = $options{$alias};
        $intlog->write($dl7, '$value: ', $value );
        my $optval = (ref $value eq 'ARRAY' ? join("", @$value) : $value );
        $intlog->write($dl7, '$optval: ', $optval );
        if ( $optspec =~ /^\=/ ) {
            $intlog->write(D_SPEW, 'REQUIRED: ', $opt );
            if ( not defined $optval or ($optval ne '0' and not $optval) ) {
                $intlog->write(D_SPEW, 'REQUIRED AND MISSING: ', $opt );
                push @missing, $opt;
            }
        }
    }
    $intlog->write($dl7, '%options: ', \%options );
    $intlog->write($dll, '@missing: ', \@missing );
    return @missing;
}
use Text::Wrap;
$Text::Wrap::columns = 70;
$Text::Wrap::huge = 'overflow';
my %already_decoded;
sub usage {
    # I really need to clean up how this gets its arguments, prubably aught to have the @_ be HASHable { optargs => \%optargs, ... } instead of trying to rely on this ordering crap
    my $optargs_orig = shift;
    my $optargs = { %$optargs_orig };
    $intlog->write($dll, '$optargs: ', $optargs );
    
    my $options = ref $_[0] eq 'HASH' ? (shift) : {};
    $intlog->write($dll, '$options: ', $options );
    my $optconfig = ref $_[0] eq 'HASH' ? (shift) : {};
    $intlog->write($dll, '$optconfig: ', $optconfig );
    my $config    = ref $_[0] eq 'HASH' ? (shift) : {};
    $intlog->write($dll, '$config: ', $config );
    # give some easy usage/help options
    if ( not $config->{nohelp} or exists $config->{help} and $config->{help} ) {
        $optargs->{help}  ||= $optargs->{usage} ||= [''   , sub { usage( $optargs, $options, $optconfig, $config ) }, 'print help message and exit'];
        $optargs->{usage} ||= $optargs->{help}  ||= [''   , sub { usage( $optargs, $options, $optconfig, $config ) }, 'print help message and exit'];
    }
#    if ( not $config->{nohelp} or exists $config->{help} and $config->{help} ) {
#        $optargs->{help}  ||= $optargs->{usage} ||= [''   , sub { usage( $optargs ) }, 'print help message and exit'];
#        $optargs->{usage} ||= $optargs->{help}  ||= [''   , sub { usage( $optargs ) }, 'print help message and exit'];
#    }
    my $no_missing  = scalar (grep { /no_missing/ } map { defined $_ ? $_ : (); } @_) ? 1 : 0;
    my $usage_args  = ref $_[0] eq 'HASH' ? (shift) : $_[0] ? $_[0] : {};
    $intlog->write($dll, '$usage_args: ', $usage_args );
    ref $usage_args eq 'HASH' or $usage_args = { $usage_args => (scalar @_ ? @_ : 1 )};
    my %options = $options ? %$options : map { ($_ => $optargs->{$_}[1]) } keys %$optargs;
    $intlog->write($dll, '$optargs: ', $optargs );
    my @missing = optargs_missing( $optargs, $options );
    $intlog->write($dll, '@missing: ', \@missing);
    my @required = map { my $a = $optargs->{$_}[0];
                         $a =~ /^\=/ ? $_ : ();
                     } sort { $a cmp $b } keys %$optargs;
    
    my @optional = map { my $a = $optargs->{$_}[0];
                         $a =~ /^\=/ ? () : $_;
                     } sort { $a cmp $b } keys %$optargs;
    
    my $missing = scalar @missing ? "MISSING REQUIRED ARGUMENT(S): ( " . join(', ', map { ($optargs->{$_} ? "--$_" : $_); } sort @missing ) . " ) ... \n" : '';
    my $base_name = $pathinfo[2];
    $intlog->write($dll, '$base_name: ', $base_name );
    my %type_spec = qw( f FLOAT
                        i INTEGER
                        s STRING
                      );
    my $indent = '     |  ';
    my $sep = ( ' ' x ((length $indent) - 4) ) . ('-' x ( $Text::Wrap::columns - ((length $indent) + 2)));
    $sep = '    ';
    $usage_args->{brief_info} ||= $config->{brief_info} || '';
    $intlog->write(CLEAN,(#'_' x ($Text::Wrap::columns + 3),
                          "\n",
                          ($config->{usage_message} ? $config->{usage_message} : () ),
                          "\n",
                          $no_missing ? () : ($missing,"\n"),
                          space("usage: $base_name $usage_args->{brief_info}", ), "\n",
                          $sep,#"\n    ",
                          join("\n" . '    ', #(' ' x (length "usage: $base_name")),
                               map {
                                   my $val = ( defined $options{$_}
                                               ? $options{$_}
                                               : $optargs->{$_}[1]
                                             );
                                   ref $val eq 'SCALAR' and $val = $$val;
                                   ref $val eq 'ARRAY'  and $val = join(', ', @$val);
                                   ref $val eq 'HASH'   and $val = join(', ', map { "$_ => $val->{$_}" } keys %$val);
                                   
                                   my $label = $_;
                                   $label =~ /\|/ and $label = '(' . $label . ')';
                                   my $required = $optargs->{$_}[0] =~ /^\=/ ? 1 : 0;
                                   #my $boolean  = $optargs->{$_}[0] =~ /(^$)|(\+)/ ? 1 : 0;
                                   $optargs->{$_}[0] =~ /([fis])/;
                                   my $type     = $1;
                                   my $boolean  = $type ? 0 : 1;
                                   $type = $boolean ? 'BOOLEAN' : $type_spec{$type};
                                   my $desc     = $optargs->{$_}[2] || $_; #'NO DESCRIPTION PROVIDED';
                                   my @desc = split ('\s+', $desc );
                                   my $show_req = ($required ? "REQUIRED($type)" : "OPTIONAL($type)");# . "\n";
                                   my @wrap = wrap( $indent, $indent, @desc);
                                   if( scalar @wrap == 1 ) {
                                       @wrap = split("\n", $wrap[0]);
                                   }
                                   $intlog->write($dll, '@wrap: ', \@wrap );
                                   $desc = "\n" . join( "\n", map { space($_, $Text::Wrap::columns ) . ' |'; } @wrap ) . "\n" . (' ' x length $indent ) . ('-' x ($Text::Wrap::columns - length $indent)) . $sep;
                                   if ( 'CODE' eq ref $val
                                        and $_ !~ /(usage|help)/
                                        and not $already_decoded{$val}++
                                      ) {
                                       $val = &$val( $_, $options{$_} );
                                   }
                                   if ( defined $val and ( $val or $val eq '0' ) ) {
                                       $val = $boolean ? $val : "'$val'";
                                   }
                                   my $onoff = ($boolean and $val) ? '#ON#' : '#OFF#';
                                   my @opt = ( $required
                                               ? ( space($show_req, 25), '   ', ( '--', space($_, 25),($val ? '   ': '***'), space($val ? ($boolean ? $onoff : $val) : ($boolean ? $onoff : uc "<$_>***"), 15)) , '   ', $desc )
                                               : ( space($show_req, 25), ' [ ', ( '--', space($_, 25),                       space($val ? ($boolean ? $onoff : $val) : ($boolean ? $onoff : uc "<$_>"),    15)) , ' ] ', $desc )
                                             );
                                   join('', @opt);
                               } #( sort
                               ( # sort here to make required options come first, and sort alphabetically in each sub-category: (required, not-required)
                                ( sort @required ),
                                ( sort @optional ),
                               )
                               #)
                              ),
                          "\n",
                          #'_' x ($Text::Wrap::columns + 3),
                         )
                  );
    %already_decoded = ();
    exists $config->{exit} or exit -1;
    $config->{exit} and exit -1;
}

#sub _prepare_message {
#    my $self  = shift;
#    my $level = shift;
#    my $args  = shift;
#    my @inmsg = @_;
#    my $dump_refs = exists $args->{dump_refs} ? $args->{dump_refs}
#      :  exists $self->{dump_refs} ? $self->{dump_refs}
#        : $level eq 'SPEW';
#    my @outmsg = ();
#    my $tmp;
#
#    $level = $args->{level} || $level;
#    my $log_level = $args->{log_level} || $self->{log_level} || $ENV{LOG_LEVEL};
#    print STDERR __LINE__, " LOG_LEVEL='$log_level', LEVEL='$level', \$args->{prefix}='$args->{prefix}'\n" if ($ENV{LOG_INTERNAL_DEBUG} > 2);
#    my $prefix = exists $args->{prefix}             ? $args->{prefix}
#      :                 $log_level =~ /^D_/         ? \&_prefix_dev
#        :                   $level =~ /CLEAN/       ? ''
#          :             defined $self->{prefix}     ? $self->{prefix}
#            :                $level =~ /^D/         ? \&_prefix_dev
#              :         $log_level =~ /(SPEW)/      ? \&_prefix_dev
#                #:           $level =~ /QUIT/        ? \&_prefix_dev
#                :           $level =~ /CRIT/        ? \&_prefix_dev
#                  :         $level =~ /FATAL/       ? \&_prefix_dev
#                    :         $level =~ /FAIL/      ? \&_prefix_dev
#                      : \&_prefix_default;
#    my @prefix;
#    my @prefix_out;
#    my $add_dev_prefix;
#    my $log_file = $args->{log_file} || $self->log_file( $level ) || $self->log_file();
#    if ( exists $args->{prefix}
#         and $log_level =~ /^D_/
#         and $log_file  =~ /^(STDOUT|STDERR)$/
#       ) {
#        $add_dev_prefix = 1;
#    }
#    push @prefix, \&_prefix_dev if $add_dev_prefix;
#    push @prefix, $prefix       if defined $prefix;
#    # really we should have somethings that checks the %args for ALL of the possible settings
#    my $st = $STACK_TRACE;
#    $STACK_TRACE = exists $args->{stack_trace} ? $args->{stack_trace}
#      : defined $self->{stack_trace}           ? $self->{stack_trace}
#        : $STACK_TRACE;
#    
#    my $code_resolve_cnt_max = 10;
#    foreach my $p ( @prefix ) {
#        my $code_resolve_cnt = 0;
#      CORE_PREFIX:
#        while ( ref $p eq 'CODE' ) {
#            $p = &$p( $level, $args );
#            last CODE_PREFIX if ( $code_resolve_cnt++ >  $code_resolve_cnt_max );
#        }
#        unshift @inmsg, $p;
#        #unshift @prefix_out, $p;
#    }
#    $STACK_TRACE = $st;# restore the previous setting
#
#    #  my $prefix_length = [ split("\n", join( '', @prefix_out)) ];
#    #  $prefix_length = $prefix_length->[-1];
#    #  $prefix_length = length $prefix_length;
#    my ($msg, $d);
#  INMSG: while ( scalar @inmsg ) {
#        $tmp = undef;
#        $msg = shift @inmsg;
#        defined $msg or $msg = 'undef';#'(UNDEFINED ELEMENT IN LOG MESSAGE ARGUMENTS)';
#        my $code_resolve_cnt = 0;
#      CHECK_REF:
#        if (( my $ref = ref $msg ) and $dump_refs ) {
#            # this next line of cruft is here so you can pass arguments to ->dump() without having to prepend with a minus sign
#            my @extra_args = map { $_ =~  /^(terse|deep|pure|id|indent)$/ ? ( "-$_" => $args->{$_} ) : ( $_ => $args->{$_} ) } keys %$args;
#            (print STDERR $this_package, " STDERR ", __LINE__, " ::: OH MY!:: ", __PACKAGE__->_caller(), $self->dump(-d=> [\@extra_args], -n =>['extra_args']), "\n") if ( $ENV{LOG_INTERNAL_DEBUG} > 4 );
#            if ( $ref eq 'CODE' ) {
#                $d = &$msg();
#                $msg = $d;
#                goto CHECK_REF unless ( ref $msg eq 'CODE' and $code_resolve_cnt++ >  $code_resolve_cnt_max );
#            } else {
#                #$d = $self->dump(-d=>[$msg],-n=>["$msg"], -deep => 0, @extra_args, -pad => (' ' x  ( $prefix_length + length $msg) ));
#                #$d = $self->dump(-d=>[$msg],-n=>["$msg"], -deep => 0, @extra_args, -pad => (' ' x  ( $prefix_length) ));
#                #$d =~ s/^\s+//;
#                #$d = "\n" . $self->dump(-d=>[$msg],-n=>["$msg"], -deep => 0, @extra_args, -pad => (' ' x  $prefix_length ) );
#                #$d = "\n" . $self->dump(-d=>[$msg],-n=>["$msg"], -deep => 0, -indent => 1, @extra_args );
#                $d = "\n" . $self->dump(-d=>[$msg],-n=>["$msg"], -deep => 0, -indent => 1, @extra_args );
#            }
#            $msg = $d;
#        }
#        push @outmsg, $msg;
#    }
#    if ( $add_dev_prefix
#         and $outmsg[-1] !~ /\n$/ms
#       ) {
#        push @outmsg, "\n";
#    };
#    return @outmsg;
#}

*_prefix_default = \&_prefix_prod;

sub _time {
    my @lt = localtime();
    #(   0,   1,    2,    3,   4,    5,    6,    7,     8)
    #($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) =
    join('',$lt[5]+1900, map { length $_ < 2 ? "0$_" : $_; } (($lt[4]+1),($lt[3])) ) . ' ' . join('', map { length $_ < 2 ? "0$_" : $_;} @lt[2,1,0]),
}

sub _prefix_prod {
    print STDERR __LINE__, " 'prefix_prod'\n" if ($ENV{LOG_INTERNAL_DEBUG} > 2);
    my $level = shift;
    return '['.join('][',map { space(pad($_, $p_pad), $p_space), }
                    "$username\@$hostname:$$",
                    _time(),
                    uc $level
                   )."] "
                     ;
}

sub _prefix_brief {
    print STDERR __LINE__, " 'prefix_brief'\n" if ($ENV{LOG_INTERNAL_DEBUG} > 2);
    my $level = shift;
    return '['.join('][',map { space(pad($_, $p_pad), $p_space), }
                    "$username\@$hostname:$$",
                    _time(),
                   )."] "
                     ;
}

sub _prefix_dev {
    print STDERR __LINE__, " 'prefix_dev_long'\n" if ($ENV{LOG_INTERNAL_DEBUG} > 2);
    my $level = shift;
    my $args  = shift;
    my $backstack = $args->{backstack} || 0;
    #"$username\@$hostname:$$:$path_abbrev:$path_base",
    my $return = '['.join('][',map { space(pad($_, $p_pad), $p_space), }
                          #__PACKAGE__->_caller($backstack + 3), # we need a 3 here to ignore (skip over) the subroutine calls within the logging module itself
                          # was 3 before we inlined something
                          __PACKAGE__->_caller($backstack + $prefix_dev_backstack), # we need a 2 here to ignore (skip over) the subroutine calls within the logging module itself
                         )."] "
                           . "\n"
                             ;
    $return .= _prefix_prod( $level, $args, @_ );
    $return .= "\n"  if ( $level =~ /CLEAN/ );
    return $return;
}

my %level_cache = ();
sub _check_level {
    my $self  = shift;
    my $msg = shift;
    my $args = {};
    my $level = shift @$msg;
    ref $level eq 'HASH'
      and ($args=$level)
        and $level=shift @$msg;
    my $map_level = $level;
    $map_level =~ s/^D_//;
    #print "LEVEL    : '$level'\n";
    #print "MAP_LEVEL: '$map_level'\n";
    $args->{log_file} ||= $self->{"log_file_$level"} || $self->{"log_file"};
    my $log_level = $args->{log_level} ||= $self->{log_level}   || DEFAULT;
    my $map_log_level = $log_level;
    $map_log_level =~ s/^D_//;
    my ( $_level, $_log_level ) = @LOG_CODE{$map_level, $map_log_level};
    print STDERR "\nLEVELS: $log_level:$map_log_level:$_log_level ... $level:$map_level:$_level\n" if $ENV{LOG_LEVEL_DEBUG};
    
    if ( not defined $_level ) {
        $intlog->write({stack_trace => 1 }, ERROR, "Illegal log level '$level' setting it to 'DEFAULT'");
        unshift @$msg, ( $level = 'DEFAULT' );
        return $self->_check_level( $msg ) unless exists $level_cache{$level};
        $intlog->write( ERROR, "Illegal log level '$level' trouble setting it to $level");
        return undef;
    }
    
    my @return =  ($level, $_level, $log_level, $_log_level, $args);
    #_actually_log( $self, -level => LOUD, -FH => \*STDOUT, -message => \@return );
    return @return;
}

sub write {
    #print STDOUT $this_package, " STDOUT ", __LINE__, " ::: OH MY! ... ", $intlog->dump([ \@_ ]);
    
    #  print STDERR $this_package," :: ", join(', ', caller()), "\n";
    my $self  = shift;
    ref $self or $self = $log;
    (print STDOUT $this_package, " STDOUT ", __LINE__, " ::: OH MY!:: ", __PACKAGE__->_caller(), $self->dump(-d=> [$_[0]], -n =>['_args']), "\n") if ( $ENV{LOG_INTERNAL_DEBUG} > 4 );
    my @msg = @_;
    #my ($level, $_level, $log_level, $_log_level, $args)  = $self->_check_level( \@msg );
    my ($level, $_level, $log_level, $_log_level, $args);
    my $use_level;
    my $map_level;
    CHECK_LEVEL:
    #sub _check_level {
    {
        #my $self  = shift;
        #my $msg = shift;
        my $msg = \@msg;
        #my $args = {};
        $args = {};
        #my $level = shift @$msg;
        $level = shift @$msg;
        ref $level eq 'HASH'
          and ($args=$level)
            and $level=shift @$msg;

        $use_level = $args->{level} || $level;
        $map_level = $use_level;
        $map_level =~ s/^D_//;
        #print "LEVEL    : '$level'\n";
        #print "MAP_LEVEL: '$map_level'\n";
        $args->{log_file} ||= $self->{"log_file_$level"} || $self->{"log_file"};
        $log_level = $args->{log_level} || $self->{log_level} || $ENV{LOG_LEVEL} || 'DEFAULT';
        my $map_log_level = $log_level;
        $map_log_level =~ s/^D_//;
        #my ( $_level, $_log_level ) = @LOG_CODE{$map_level, $map_log_level};
        ( $_level, $_log_level ) = @LOG_CODE{$map_level, $map_log_level};
        print STDERR "\nLEVELS: $log_level:$map_log_level:$_log_level ... $level:$map_level:$_level\n" if $ENV{LOG_LEVEL_DEBUG};
        
        if ( not defined $_level ) {
            $intlog->write({stack_trace => 1 }, ERROR, "Illegal log level '$level' setting it to 'DEFAULT'");
            unshift @$msg, ( $level = 'DEFAULT' );
            #return $self->_check_level( $msg ) unless exists $level_cache{$level};
            if ( not exists $level_cache{$level} ) {
                goto CHECK_LEVEL;
                #($level, $_level, $log_level, $_log_level, $args) = $self->_check_level( $msg );
            } else {
                $intlog->write( ERROR, "Illegal log level '$level' trouble setting it to $level");
                return undef;
            }
        }
        
#        my @return =  ($level, $_level, $log_level, $_log_level, $args);
#        #_actually_log( $self, -level => LOUD, -FH => \*STDOUT, -message => \@return );
#        return @return;
    }
    # this needs to be set up to log at any of severa levels which may be set simultaneously
    # eg log at WARN and TRACE
    # log levels should be a list
    # ie @_log_levels rather than $_log_level
    my $backstack = $args->{backstack} || 0;
    my $return = \@msg;
    my $status = 1;
    (print STDERR 'XXXXXX  ', $this_package, " STDERR ", __LINE__, " ::: OH MY! status=$status ... \$ALWAYS_LOG{$map_level}: '", $ALWAYS_LOG{$map_level}, "' :: ", __PACKAGE__->_caller(), $self->dump(-d=> [$args], -n =>['args']), "\n") if ( $ENV{LOG_INTERNAL_DEBUG} > 4 );
    if( not $ALWAYS_LOG{$map_level} ) {
        if ( my $e = $self->{exclusive} ) {
            $level =~ /$e/
              or $status = 0;# or return join( '', @$return );
        } else {
            $_level >= $_log_level 
              or $status = 0;
            #or return join( '', map { defined $_ ? $_ : 'undef' } @$return );
        }
    }
    (print STDERR 'XXXXXX  ', $this_package, " STDERR ", __LINE__, " ::: OH MY! status=$status ... \$ALWAYS_LOG{$map_level}: '", $ALWAYS_LOG{$map_level}, "' :: ", __PACKAGE__->_caller(), $self->dump(-d=> [$args], -n =>['args']), "\n") if ( $ENV{LOG_INTERNAL_DEBUG} > 4 );
    if ( #not $ALWAYS_LOG{$map_level} and
        $status
        and my $packages = $self->{packages}
       ) {
        my $do_match;
        my $dont_match;
        my $do_log_rx   = $packages->[0];
        my $dont_log_rx = $packages->[1];

        my $log_called_package = _log_called_package(1)->[0];
        #print STDERR __PACKAGE__, ":", __LINE__, ": ", "LOG CALLED PACKAGE: '$log_called_package'\n";
        if ( scalar @$do_log_rx ) {
            foreach my $do_rx ( @$do_log_rx ) {
                if ( $log_called_package =~  /^($do_rx)$/ ) {
                    #print STDERR "DO LOG: $do_log_rx\n";
                    #$do_match = ( $do_match and length $do_match > length $do_rx ) ? $do_match : $do_rx;
                    $do_match = $do_rx;
                }
            }
            $do_match or $status = 0;
        }
        
        if ( $status and scalar @$dont_log_rx ) {
            foreach my $dont_rx ( @$dont_log_rx ) {
                if ( $status
                     #and not $do_match
                     #and ( not $do_match or ( $dont_log_rx =~ /$do_log_rx/ ))
                     and $log_called_package =~  /^($dont_rx)$/
                   ) {
                    #$dont_match = ( $dont_match and length $dont_match > length $dont_rx ) ? $dont_match : $dont_rx;
                    $dont_match = $dont_rx;
                    $status = 0;
                }
            }
        }
        
        if ( $do_match and $dont_match ) {
            # if it matches on both DO and DONT, what are we supposed to do? Here we simply say that the match with the lengthiest regex wins
            $status = ( length $do_match > length $dont_match ) ? 1 : 0 ;
            print STDERR __PACKAGE__, ":", __LINE__, ": ", "DO   status=$status ($do_match): $do_log_rx\n"     if $ENV{LOG_PACKAGES_DEBUG};
            print STDERR __PACKAGE__, ":", __LINE__, ": ", "DONT status=$status ($dont_match): $dont_log_rx\n" if $ENV{LOG_PACKAGES_DEBUG};
        }
    }
    
    print STDERR __LINE__, " LOG_LEVEL='$log_level', LEVEL='$level', MAP_LEVEL='$map_level', \$args->{prefix}='$args->{prefix}'\n" if ($ENV{LOG_INTERNAL_DEBUG} > 2);
    if ( $status ) {
        #warn "STATUS: $status ::: $level:$_level ... $log_level:$_log_level";
        # this is an effort at in-lining some subroutines
        #@msg = $self->_prepare_message( $level, $args, @msg );
        #sub _prepare_message {
        {
#            my $self  = shift;
#            my $level = shift;
#            my $args  = shift;
#            my @inmsg = @_;
            my @inmsg = @msg;
            my $dump_refs = exists $args->{dump_refs} ? $args->{dump_refs}
              :  exists $self->{dump_refs} ? $self->{dump_refs}
                : $level eq 'SPEW';
            my @outmsg = ();
            my $tmp;
            
            my $prefix = exists $args->{prefix}             ? $args->{prefix}
              :                 $log_level =~ /^D_/         ? \&_prefix_dev
                :               $use_level =~ /CLEAN/       ? ''
                  :             defined $self->{prefix}     ? $self->{prefix}
                    :           $use_level =~ /^D/          ? \&_prefix_dev
                      :         $log_level =~ /(SPEW)/      ? \&_prefix_dev
                        #:       $use_level =~ /QUIT/        ? \&_prefix_dev
                        :       $use_level =~ /CRIT/        ? \&_prefix_dev
                          :     $use_level =~ /FATAL/       ? \&_prefix_dev
                            :   $use_level =~ /FAIL/        ? \&_prefix_dev
                              : \&_prefix_default;
            my @prefix;
            my @prefix_out;
            my $add_dev_prefix;
            my $log_file = $args->{log_file} || $self->log_file( $level ) || $self->log_file();
            if ( exists $args->{prefix}
                 and $log_level =~ /^D_/
                 and $log_file  =~ /^(STDOUT|STDERR)$/
               ) {
                $add_dev_prefix = 1;
            }
            push @prefix, \&_prefix_dev if $add_dev_prefix;
            push @prefix, $prefix       if defined $prefix;
            my $code_resolve_cnt = 0;
            my $code_resolve_cnt_max = 10;
            # really we should have somethings that checks the %args for ALL of the possible settings
            my $st = $STACK_TRACE;
            $STACK_TRACE = exists $args->{stack_trace} ? $args->{stack_trace}
              : defined $self->{stack_trace}           ? $self->{stack_trace}
                : $STACK_TRACE;
            
            foreach my $p ( @prefix ) {
              CODE_PREFIX:
                while ( ref $p eq 'CODE' ) {
                    $p = &$p( $level, $args );
                    last CODE_PREFIX if ( $code_resolve_cnt++ >  $code_resolve_cnt_max );
                }
                unshift @inmsg, $p;
                #unshift @prefix_out, $p;
            }
            $STACK_TRACE = $st;# restore the previous setting

            #  my $prefix_length = [ split("\n", join( '', @prefix_out)) ];
            #  $prefix_length = $prefix_length->[-1];
            #  $prefix_length = length $prefix_length;
            my ($msg, $d);
          INMSG: while ( scalar @inmsg ) {
                $tmp = undef;
                $msg = shift @inmsg;
                defined $msg or $msg = 'undef';#'(UNDEFINED ELEMENT IN LOG MESSAGE ARGUMENTS)';
                my $code_resolve_cnt = 0;
              CHECK_REF:
                if (( my $ref = ref $msg ) and $dump_refs ) {
                    # this next line of cruft is here so you can pass arguments to ->dump() without having to prepend with a minus sign
                    my @extra_args = map { $_ =~  /^(terse|deep|pure|id|indent)$/ ? ( "-$_" => $args->{$_} ) : ( $_ => $args->{$_} ) } keys %$args;
                    (print STDERR $this_package, " STDERR ", __LINE__, " ::: OH MY!:: ", __PACKAGE__->_caller(), $self->dump(-d=> [\@extra_args], -n =>['extra_args']), "\n") if ( $ENV{LOG_INTERNAL_DEBUG} > 4 );
                    if ( $ref eq 'CODE' ) {
                        $d = &$msg();
                        $msg = $d;
                        goto CHECK_REF unless ( ref $msg eq 'CODE' and $code_resolve_cnt++ >  $code_resolve_cnt_max );
                    } else {
                        #$d = $self->dump(-d=>[$msg],-n=>["$msg"], -deep => 0, @extra_args, -pad => (' ' x  ( $prefix_length + length $msg) ));
                        #$d = $self->dump(-d=>[$msg],-n=>["$msg"], -deep => 0, @extra_args, -pad => (' ' x  ( $prefix_length) ));
                        #$d =~ s/^\s+//;
                        #$d = "\n" . $self->dump(-d=>[$msg],-n=>["$msg"], -deep => 0, @extra_args, -pad => (' ' x  $prefix_length ) );
                        #$d = "\n" . $self->dump(-d=>[$msg],-n=>["$msg"], -deep => 0, -indent => 1, @extra_args );
                        $d = "\n" . $self->dump(-d=>[$msg],-n=>["$msg"], -deep => 0, -indent => 1, @extra_args );
                    }
                    $msg = $d;
                }
                push @outmsg, $msg;
            }
            if ( $add_dev_prefix
                 and $outmsg[-1] !~ /\n$/ms
               ) {
                push @outmsg, "\n";
            };
            #return @outmsg;
            @msg = @outmsg;
        }

        $n = exists $args->{n} ? $args->{n} : ($self->{n} || "\n");
        (print STDERR $this_package, " STDERR ", __LINE__, " ::: OH MY! ... \$ALWAYS_LOG{$use_level}: '", $ALWAYS_LOG{$use_level}, "' :: ", __PACKAGE__->_caller(), $self->dump(-d=> [$args], -n =>['args']), "\n") if ( $ENV{LOG_INTERNAL_DEBUG} > 4 );
        unless ( $args->{dont_actually_log} ) {
            #$return = $self->_actually_log( %$args, -level => $use_level, -message => $return );
            %$args = ( %$args, -level => $level, -message => $return );
            #sub _actually_log {
            {
                
                #print STDERR $this_package, " ", __LINE__, " ::: OH MY! ... ", $_[0]->dump([ \@_ ]);
                #my $self    = shift;
                #(warn $this_package, " STDOUT ", __LINE__, " ::: OH MY!:: ", __PACKAGE__->_caller(), $self->dump(-d=> [\@_], -n =>['_']), "\n") if $ENV{LOG_INTERNAL_DEBUG};
                #my $args = { @_ };
                $args->{-terse}   ||= $self->{terse};
                $args->{-level}   ||= INFO;
                $args->{-message} ||= ' - -- NO MESSAGE -- - ';
                my $fh = $self->fh( %$args );
                if ( not $fh ) {
                    my $log_file = $self->log_file($args->{-level});

                    my $error_level = FATAL;
                    if ( not $log->handle_fatals() ) {
                        $error_level = ERROR;
                    }
                    $intlog->write($error_level, "No filehandle for `", $args->{-level}, "' on `", $log_file, "'",  \%FHS_NA);
                    exit 1 if $log->handle_fatals();
                    #return undef;
                    $return = undef;
                }
                else {
                    #print "MESSAGE: $message\n";
                    #return $self->_WRITE( %$args, -FH => $fh );
                    $return = $self->_WRITE( -FH => $fh, %$args );
                }
            };

            defined $return or $status = undef;
        }
        #    if ( $use_level eq MESSAGE  ) {
        #      if ( my $email = $args->{email} ? $args->{email} : $self->{email} ) {
        #        # we should send a message to the bloke?
        #      } else {
        #        #$intlog->write(ERROR, "No email address specified to send MESSAGE: $return");
        #        $self->write(ALERT, "No email address specified to send MESSAGE: $return") unless $self->{DEBUG}{NO_ALERT};
        #      }
        #    }
        $n = undef;
    }
    ref $return eq 'ARRAY' and $return = join('', map { defined $_ ? $_ : 'undef' } @$return);
    #print STDOUT $this_package, " STDOUT ", __LINE__, " ::: OH MY! ... ", $intlog->dump([ \@_ ]);
    return wantarray ? ( $status, $return ) : $status ;
}

sub _actually_log {
    #print STDERR $this_package, " ", __LINE__, " ::: OH MY! ... ", $_[0]->dump([ \@_ ]);
    my $self    = shift;
    #(warn $this_package, " STDOUT ", __LINE__, " ::: OH MY!:: ", __PACKAGE__->_caller(), $self->dump(-d=> [\@_], -n =>['_']), "\n") if $ENV{LOG_INTERNAL_DEBUG};
    my $args = { @_ };
    $args->{-terse}   ||= $self->{terse};
    $args->{-level}   ||= INFO;
    $args->{-message} ||= ' - -- NO MESSAGE -- - ';
    my $fh = $self->fh( %$args );
    unless ( $fh ) {
        my $log_file = $self->log_file($args->{-level});

        my $error_level = FATAL;
        if ( not $log->handle_fatals() ) {
            $error_level = ERROR;
        }
        $intlog->write($error_level, "No filehandle for `$args->{-level}' on $log_file");
        #exit 1;
        return undef;
    }
    #print "MESSAGE: $message\n";
    return $self->_WRITE( %$args, -FH => $fh );
};

#@f{qw(package filename line subroutine hasargs wantarray evaltext is_require hints bitmask )}=caller();
my @showf = qw(package filename line subroutine hasargs wantarray evaltext is_require );
sub called_from {
    my $self = shift;
    my $f    = exists $_[0] ? (shift) : (( ref $self ? 2 : $self ) || 2);
    $intlog->write($dll, '$f: ', $f );
    my $lcpa = $self->_log_called_package( $f );
    $intlog->write($dll, '$lcpa: ', $lcpa );
    my $lcp = $lcpa->[0];
    $intlog->write($dll, '$lcp: ', $lcp );
    return $lcp;
}

sub _log_called_package {
    my $self = shift;
    my $f    = shift || ( ref $self ? 0 : $self ) || 0;
    my $nf   = $f + 1;
    my $log_called_package = '';
    my $log_called_file = '';
    my @caller = ();
    my @f = caller($f);
    my ( $package, $filename, $line, $subroutine ) = @f;
    #print '( $package, $filename, $line, $subroutine ) = ', "( $package, $filename, $line, $subroutine ) [$f]\n";
    my @nf = caller($nf);
    my ( $npackage, $nfilename, $nline, $nsubroutine ) = @nf;
    #print '( $npackage, $nfilename, $nline, $nsubroutine ) = ', "( $npackage, $nfilename, $nline, $nsubroutine ) [$nf]\n";
    if ( $nsubroutine ) {
        $log_called_package = "$nsubroutine:$line";
        $log_called_file    = "$filename:$line";
    } elsif ( $package ) {
        $log_called_package = "$package:$line";
        $log_called_file    = "$filename:$line";
    }
    return [ $log_called_package, $log_called_file, \@f, \@nf ];
}

sub _caller {
    my $self = shift;
    my $f    = shift || 0;
    my @caller = ();
    if ( $STACK_TRACE ) {
        # I wonder if there is a single call to give me a stack trace like I want, I know Carp will cluck() but why didn't I use that in the first place?
        # did I just do my own for some easier to read formatting?
        my $s = 0;
        my %mes;
        my @mes = ({map{$mes{$_}=!$mes{$_}?length$_:($mes{$_}<length$_)?length$_:$mes{$_};($_=>$_);}@showf});
        my $width = 0;
        my $depth = 0;
        while (1) {
            my %f;
            $depth = $f + ++$s;
            @f{ @showf, qw( hints bitmask )}= caller($depth);
            # this is probably a stupid way to break out of this loop, we basically keep stepping back up the stack until there is nothing left
            last unless join('',map{$f{$_}?$f{$_}:''}(@showf));
            $width=0;
            my $x = 0;
            #push @mes, "$s => \n\t", join("\n\t",map{(space( $_ . "(" . $x++ . ")") . " => " . ($f{$_}?$f{$_}:'undef')) }@showf), "\n";
            foreach (@showf) {$f{$_} ||= 'undef';$mes{$_}=!$mes{$_}?length$f{$_}:($mes{$_}<length$f{$_})?length$f{$_}:$mes{$_};$width+=$mes{$_};}
            $mes[$depth] = \%f;
        }
        my ($c, @c);
        my $sep = '';
        my @m = ( '_' x $width,"\n", join("", "\n", map { if($_){$c=$_;@c=map{(space($c->{$_},$mes{$_}+2,$sep) . ' | ');}@showf;$sep='';(@c,"\n");}else{()}}@mes),'_' x $width,"\n",);
        push @caller, @m;
    }
    my $log_called_f       = _log_called_package( $f );
    print STDERR 'log_called_f: ', $self->dump( [ $log_called_f ] ), "\n" if $ENV{LOG_DEBUG};
    my $log_called_at      = _log_called_package( $f + 1 );
    print STDERR 'log_called_at: ', $self->dump( [ $log_called_at ] ), "\n" if $ENV{LOG_DEBUG};
    if ( not $log_called_at->[0] ) {
        $log_called_at = $log_called_f;
    }
    my $called_called_from = _log_called_package( $f + 2 );
    if ( not $called_called_from->[0] ) {
        $called_called_from = $log_called_at;
    }
    print STDERR 'log_called_f: ', $self->dump( [ $log_called_f ] ), "\n" if $ENV{LOG_DEBUG};
    print STDERR 'log_called_at: ', $self->dump( [ $log_called_at ] ), "\n" if $ENV{LOG_DEBUG};
    print STDERR 'called_called_from: ', $self->dump( [ $called_called_from ] ), "\n" if $ENV{LOG_DEBUG};
    push @caller, "log call at $log_called_at->[0] in file $log_called_at->[1]";
    push @caller, "$log_called_at->[0] called from $called_called_from->[0] in file $called_called_from->[1]";
    return wantarray ? @caller : join('', @caller );
}

LOGS: { # a cache of open log objects for output
    # this may not be too desirable in the end because 
    # you lose individual control of the log level, file ... and such
    # although I may be able to fix that
    my %LOGS = ( STDOUT  => $this_package->object( { log_file  => 'STDOUT', log_level => $log_level } ),
                 STDIN   => $this_package->object( { log_file  => 'STDIN' , log_level => $log_level } ),
                 STDERR  => $this_package->object( { log_file  => 'STDERR', log_level => $log_level } ),
               );
    
    # unless otherwise specified we will use STDERR as our output stream
    $LOGS{DEFAULT} = $LOGS{$default_fh};
    #use Carp qw( cluck confess );
    #local $SIG{__WARN__} = \&cluck;
    #local $SIG{__DIE__} = \&confess;
    
    sub object {
        # there should probably be a better way of specifying which existing
        # logging object should be used rather than REALLOG
        my $self = shift;
        my $class = ref $self || $self;
        #carp(  " --  $self->object() CALLER  -- " );
        $self = $class->new( @_ ) unless ref $self;
        
        my @args = @_;
        my $args;
        
        if ( my $init = shift @args ) {
            ref $init eq 'HASH'  and $args = $init;
            ref $init eq 'ARRAY' and 1;
        }
        my $log = $args->{log} || $class || 'DEFAULT';
        $log = $LOGS{$log} ||= ($class eq $this_package ? $self : $this_package->new(@_));
        
        return $log if $log;
        # hmmm failed?
        return delete $LOGS{$log};
    }
}


#print STDERR __FILE__, ":", __LINE__, " :: \n", $log->dump( -n => [ 'FHS_NA', 'FHS_NO'], -d => [ \%FHS_NA,  \%FHS_NO]), "\n";
FILEHANDLES : { # a cache of open filehandles for output
    # I may want to split this into open_fh, get_fh, close_fh (with perhaps an argument helper of get_fh_file_args or something, to sort out the passed arguments for each of the potential functions mentioned )
    sub close_fh { # simply closes the current filehandle and removes if from the list of open handles
        my $self = shift;
        my $status = 'NA';
        if ( my $fh = $self->fh( @_, no_open => 1 ) ) {
            $intlog->write($dll, '$fh: ', $fh );
            #; # the problem here, is that if arguments were passed, and no such filehandle was already op, then fh() is going to open a filehandle, give it to us whereupon we are going to immediately close it. That kind of sucks.
            my $file_no = fileno($fh);
            my $file = $FHN_NO{$file_no};
            my $file_clean = $file;
            $file_clean =~ s/^\s*([>]{1,2})\s*//;
            if ( $ENV{LOG_DEBUG} ) {
                print STDERR "file_no='$file_no'\n";
                print STDERR "file='$file'\n";
                print STDERR "file_clean='$file_clean'\n";
            }
            if ($fh and $file_no) {
                $status = close($fh) or warn "Couldn't close filehandle on '$file': $!";
                delete $FHS_NA{$file_clean};
                delete $FHS_NO{$file_no};
                delete $FHN_NO{$file_no};
            }
        } else {
            #$intlog->write($dl7, '@_: ', \@_ );
            #die;
        }
        return $status;
    }
    *get_fh = \&fh;
    sub fh {
        # this is a bit fucky nutty, I would like to pull all of the file handle-handling stuff into another package, I would like to add hooks for on-the-fly (de)compression, preferably all in perl (making it platform independent), but with outside programs if necessary 
        #print STDERR __PACKAGE__, ":", __LINE__, "\n";
        #print STDOUT __PACKAGE__, ":", __LINE__, "\n";
        my $self = shift;
        #return $FHS_NA{STDERR};
        my $args = { @_ };
        #print STDOUT join(" ", @_), "\n";
        my $level = $args->{-level} || DEFAULT;
        my $file;
        my $fh;
        my $file_no;
        my $file_clean;
        #_WRITE( "SHITBALLS", "  \$level  = '$level'\n" );
        if ( $level =~ /^(STDERR|STDOUT)$/i ) {
            $fh = $FHS_NA{"\U$level"};
            $file_no = fileno($fh);
            $file = $level;
            $file_clean = $file;
        } else {
            $file   = $args->{"log_file_$level"};
            $file ||= $args->{log_file};
            #print STDERR __PACKAGE__, ":", __LINE__, "\n";
            #print STDOUT __PACKAGE__, ":", __LINE__, "\n";
            $file ||= $self->{"log_file_$level"};
            #print STDERR __PACKAGE__, ":", __LINE__, "\n";
            #print STDOUT __PACKAGE__, ":", __LINE__, "\n";
            $file ||= $self->{log_file};
            #print STDERR __PACKAGE__, ":", __LINE__, "\n";
            #print STDOUT __PACKAGE__, ":", __LINE__, "\n";
            $file ||= $LEVEL_FHS{$level};
            #print STDERR __PACKAGE__, ":", __LINE__, "\n";
            #print STDOUT __PACKAGE__, ":", __LINE__, "\n";
            $file ||= $default_fh;
            #print STDERR __PACKAGE__, ":", __LINE__, "\n";
            #print STDOUT __PACKAGE__, ":", __LINE__, "\n";
            $fh = $args->{fh};# || $FHS_NA{$file_clean};
            #      $file_clean = $file;
            #      $file_clean =~ s/^\s*([>]{1,2})\s*//;
            #      $fh = $args->{fh} || $FHS_NA{$file_clean};
            #print STDERR __PACKAGE__, ":", __LINE__, "\n";
            #print STDOUT __PACKAGE__, ":", __LINE__, "\n";
        }
        #fileno($fh);
        #print STDERR __PACKAGE__, ":", __LINE__, "\n";
        #print STDOUT __PACKAGE__, ":", __LINE__, "\n";
        #print STDERR "FH: [$level] :: ", $fh, ":", fileno($fh), " ::: $file $args->{log_file}\n";
        #print STDOUT "FH: [$level] :: ", $fh, ":", fileno($fh), " ::: $file $args->{log_file}\n";
        #print STDERR __PACKAGE__, ":", __LINE__, "FH: [$level] :: ", ($fh||'undef'), ":", " ::: $file_clean $args->{log_file}\n";
        #print STDOUT __PACKAGE__, ":", __LINE__, "FH: [$level] :: ", ($fh||'undef'), ":", " ::: $file_clean $args->{log_file}\n";
        my @fhs;
        my $reffh;
        if ( ref $fh eq 'ARRAY' ) {
            $reffh = 1;
            @fhs = @$fh;
        } else {
            $reffh = 0;
            @fhs = $fh;
        }
        my @return;
        if ( $fh ) {
            foreach my $_fh ( @fhs ) {
                $file_no = fileno($_fh);
                #print STDERR __PACKAGE__, ":", __LINE__, "file_no: $file_no\n";
                #print STDOUT __PACKAGE__, ":", __LINE__, "file_no: $file_no\n";
                if ( defined $file_no ) {
                    # I don't know if I should cache this here, because we may not have been responsible for opening it
                    #::# $FHS_NA{$file_clean}    = $fh;
                    #::# $FHN_NO{$file_no} = $file;
                    #::# $FHS_NO{$file_no} = $fh;
                    push @return, $_fh;
                } else {
                    warn "$!: $file";
                }
            }
            return $reffh ? \@return : $return[0];
        }
        
        my @files;
        my $reffile;
        if ( ref $file eq 'ARRAY' ) {
            $reffile = 1;
            @files = @$file;
        } else {
            $reffile = 0;
            @files = $file;
        }
        #print STDERR __FILE__, ":", __LINE__, " :: \n", $self->dump( -n => [ 'FHS_NA', 'FHS_NO', 'FHN_NO'], -d => [ \%FHS_NA, \%FHS_NO, \%FHN_NO]), "\n";
        #print STDERR __FILE__, ":", __LINE__, " :: \n", $self->dump( -n => [ 'files'], -d => [ \@files ]), "\n";
        foreach my $_file ( @files ) {
            #print STDERR __FILE__, ":", __LINE__, " :: \n", $self->dump( -n => [ '_file'], -d => [ $_file ]), "\n";
            my $_file_clean;
            $_file_clean = $_file;
            $_file_clean =~ s/^\s*(\||[>]{1,2})\s*//;
            #print STDERR __FILE__, ":", __LINE__, " :: \n", $self->dump( -n => [ '_file_clean'], -d => [ $_file_clean ]), "\n";
            my $_fh = $FHS_NA{$_file_clean};
            if ( $args->{no_open} ) {
                push @return, $_fh;
            } else {
                unless ( $_fh ) {
                    if ( fileno($_file) ) {
                        $_fh = $_file;
                    } else {
                        my $mode;
                        if ( $_file =~ /^\s*(\||[>]{1,2})/ ) {
                            $mode = $1;
                        } else {
                            $mode = -f $_file_clean ? '>>' : '>';
                        }
                        $_fh = new IO::File               or die $!;
                        print STDERR "Opening new filehandle for '$_file' on '$mode' '$_file_clean'\n" if $ENV{LOG_DEBUG};
                        my $opened = $_fh->open( "$mode$_file_clean" );
                        unless ( $opened ) {
                            my $error_level = FATAL;
                            if ( not $log->handle_fatals() ) {
                                $error_level = ERROR;
                            }
                            $intlog->write($error_level, "$mode $_file_clean : $!");
                            return undef;
                        }
                        #print STDERR "Opened new filehandle '$opened' for '$file' on '$mode' '$file_clean'\n";
                        #print STDOUT "Opened new filehandle '$opened' for '$file' on '$mode' '$file_clean'\n";
                    }
                }
                my $_file_no = fileno($_fh);
                defined $_file_no                 or die $!;
                #print STDERR "Got fileno on new filehandle '$file_no' for '$file' on '$mode' '$file_clean'\n";
                #print STDOUT "Got fileno on new filehandle '$file_no' for '$file' on '$mode' '$file_clean'\n";
                
                ################################################################################
                # this locking screwed me all up once when I was running under mod_perl
                # I think it was the exclusive lock collision between different httpd child processes
                # I should make this a per-file option I guess
                # in any case this wouldn't really work in an NFS environment, because there advisory locks are IPC based
                #my $flocked = flock $fh, LOCK_EX               or die $!;
                #print STDERR "Got lock on new filehandle '$flocked' for '$file' on '$mode' '$file_clean'\n";
                #print STDOUT "Got lock on new filehandle '$flocked' for '$file' on '$mode' '$file_clean'\n";
                ################################################################################
                
                $FHS_NA{$_file_clean} = $_fh;
                $FHS_NO{$_file_no} = $_fh;
                $FHN_NO{$_file_no} = $_file;
                #    print STDERR __PACKAGE__, ":", __LINE__, "\n";
                #    print STDOUT __PACKAGE__, ":", __LINE__, "\n";
                ( $self->{unbuffer} or $args->{unbuffer} ) and _unbuffer( $_fh );
                #    print STDERR __PACKAGE__, ":", __LINE__, "\n";
                #    print STDOUT __PACKAGE__, ":", __LINE__, "\n";
                push @return, $_fh;
            }
            #print STDERR __FILE__, ":", __LINE__, " :: \n", $self->dump( -n => [ 'FHS_NA', 'FHS_NO', 'FHN_NO'], -d => [ \%FHS_NA, \%FHS_NO, \%FHN_NO]), "\n";
        }
        return $reffile ? \@return : $return[0];
    }
    
    sub _unbuffer {
        my $fh = shift;
        my $selected = select;
        # disable buffering on this filehandle
        select $fh; $| = 1;
        # restore previously selected filehandle
        select $selected;
        return $fh;
    }
    
    sub _WRITE {
        my $self = shift;
        #print STDERR __FILE__, ":", __LINE__, " :: ", $self->dump([ \@_ ]), "\n";
        my $message;
        my $fh;
        my $args = {};
        if ( $_[0] =~ /^-/ ) {
            $args = { @_ };
            $message = $args->{-message} or return undef;
            ref $message eq 'ARRAY' or $message = [ $message ] ;
            $fh = $args->{-FH};
        } else {
            shift @_ if ( $fh = $FHS_NA{$_[0]} );
            local $STACK_TRACE = 1;
            print STDERR __FILE__, ":", __LINE__, " :: ", $self->dump([ \@_ ]), "\n";
            $message = [ join ' ', __PACKAGE__->_caller(), map { defined $_ ? $_ : 'undef'; } @_ ] ;
            exit 1;
        }
        
        my $level = $args->{-level} || CLEAN;
        
        my $return = join '', @$message;
        if ( $args->{-terse} ) {
            $return =~ s/\s+/ /mg;
        }
        
        $fh ||= $FHS_NA{$default_fh};
        my @fhs;
        my $reffh;
        if ( ref $fh eq 'ARRAY' ) {
            $reffh = 1;
            @fhs = @$fh;
        } else {
            $reffh = 0;
            @fhs = $fh;
        }
        
        foreach my $_fh ( @fhs ) {
            #print STDERR __FILE__, ":", __LINE__, " :: \n", $self->dump( -n => [ 'fh', 'FHS_NA', 'FHS_NO'], -d => [ $_fh, \%FHS_NA,  \%FHS_NO]), "\n";
            fileno($_fh) or $_fh = $FHS_NA{$_fh} or die "Invalid filehandle: " . $self->dump( -n => [ 'fh' ], -d => [ $_fh ] );
            #_lock( $_fh );
            print $_fh $return, $n or die ( "$!: arguments to _WRITE were => " . $self->dump( -n => [ 'args' ], -d => [ $args ] ));
            #_unlock( $_fh );
        }
        
        #print STDERR "level=`", ($level || 'undef'), "'\n";
        if ( $level =~ /^(CRIT|FATAL)$/ and ( defined $args->{handle_fatals} ? $args->{handle_fatals} : $self->{handle_fatals} ) ) {
            #local $STACK_TRACE = 1;
            #die $self->_caller( ) . "\n$return";
            #die "$level\n";
            die "FATAL error! $return\n";
        }

        if ( $BIG_WARN_ON{$level} ) {
          #print STDERR  "\n\n\nDOING BIG WARN ON '$level' '$ENV{BIG_WARN_ON_FATAL}'\n\n\n";
          #local $STACK_TRACE = 1;
          warn $self->_caller( ) . "\n$return";
          #die;
        }
        
        if ( $level eq QUIT ) {
            exit ($args->{QUIT} || $args->{EXIT} || $LOG_CODE{QUIT} ) unless $self->{DEBUG}{NO_QUIT};
        }
        
        return $return;
    }
}
sub _lock {
    my $fh = shift;
    #flock($fh,LOCK_EX);
    # and, in case someone appended
    # while we were waiting...
    seek($fh, 0, 2);
}

sub _unlock {
    my $fh = shift;
    #flock($fh,LOCK_UN);
}
END {
    #    delete $FHS_NA{STDERR};
    #    delete $FHS_NA{STDOUT};
    #    foreach my $fh ( values %FHS_NA ) {
    #      $fh->close();
    #    }
}

1;
__END__
=head1 NAME

Log::Easy - Easy to use, feature rich general purpose logging utility

=head1 SYNOPSIS

  use Log::Easy;
  $log = new Log::Easy;

=item $log->write([{ OPTIONS }], <LOG_LEVEL>, @message );

This is the main function for this package. If the first argument is a
hash reference, it is taken as options to the logger for this log call
ONLY, and may contain values for ANY of the options that the logger
knows how to use. The first argument after the optional OPTIONS hash
MUST BE the log level for this log. If it ends up being something that
is not a log_level, then a default log_level is assigned to the write
call. If the $log->write($log_level, @message) is equal to or greater
than the $log->log_level() [or the $OPTIONS->{log_level}] then it will
be output, otherwise it will not. 

=item $log->clone( [$data] );

make a duplicate of the supplied $data or this log object if no data
is supplied. Uses data dumper to duplicate original, therefore CODE
references are not necessarily handled, although the contents of
$log->prefix() will be assigned to the new (cloned) object from the
original.  returns the cloned object

=head2 OPTIONS

=item $log->n();

set the message terminator for this log object, each log message
output will be terminated with the contents of this setting (default "\n")

=item  $log->log();

set the log object to use for actual write operation, this occurrs
AFTER the decision as to whether or not this log message will be
processed. This allows one to set the log object to another logging
module (such as running under apache)


=item  $log->log_file();

set the output file for messages processed through this log object (
can be a file handle or file path, default => 'STDERR')


=item  $log->log_level();

set the thrshold for which messages will actiually be logged, only
messages with a log_level set to a (nuerically) higher value will be
output (default => WARN)


=item  $log->dump_refs();

set the behavior when references are encountered in the message list
contents (default => 1) if true, use data dumper to dump out the
references


=item  $log->handle_fatals();

if true, log module will terminate program execution on any log calls
marked as FATAL or CRIT (CRIT for now [maybe add EMERG], default => 1)


=item  $log->exclusive();

if true, and set to a colon delimited set of log_level indicators,
this log object will only output its message if the current log call
is for a level listed in exclusive (default => undef)


=item  $log->stack_trace();

if true (and this log message is of sufficient log_level to be
output), issue a pretty little stack trace for the log call


=item  $log->email(); #NOT YET IMPLEMENTED

for a log level of MESSAGE, send the log message to the email address
listed here, if not set, and a MESSAGE log comes in, then send the
email to the owner of the process


=item  $log->prefix();

set ther prefix that each log message will have, may be either a
string, or a CODE ref


=head1 DESCRIPTION

  This logging facility has many features developers may find handy.

=item FILTERING

One of the coolest things is FILTERING (see log_level_filtered). Using
filtering we can greatly decrease the performance penalty of copious
log entries (each requireing >1 subroutine call) by filtering out log
messages at compile time. Here is the general idea:

 # FILTERED
 $log->write($dll, "Nifty stuff");

 # NOT-FILTERED, but will issue a warning if $ENV{WARN_FILTER} is true.
 # because it looks kind of like a log_level indicator that sould be filtered ($dll)
 $log->write(dll, "Nifty stuff");

 # NOT-FILTERED, functionally equivalent to previous example
 $log->write(DEBUG, "Nifty stuff");


=head2 EXPORT

None by default.

=over 8

=item log

exports a log object into your namespace. This is most likely an
application global log object meaning that if you wan't specific
logging behavior in any given package, you'll probably want to
intanntiate a new log object

=item log_level

exports several constants into your namespace:
   DEFAULT MESSAGE LOUD CLEAN 
   EMERG ALERT CRIT FATAL ERROR WARN NOTICE INFO DEBUG
   TRACE SPEW


=item log_level_filtered

these are scalar variable ($) aliases for the constants mentioned in
log_level, the convention is that each alias begins with the same
letter as the real log_level, followed by 2 `l's (eg ERROR => $ell,
DEBUG => $dll, etc)

These log_level specifiers, when used with the leading dollar sign MAY
BE FILTERED OUT depending upon the settings for $ENV{LOG_FILTER}

=item log_level_not_filtered

in name, these are identical to the log_level_filtered variables, but
they are implemented as constants (no leading $) and they ARE NOT
SUBJECT TO FILTERING AS IS THE CASE FOR log_level_filtered

=back

=head1 HISTORY

=over 8

=item 0.01
Basic package w/ full functionality, but no docs yet

=item 0.01.1
Basic package w/ full functionality, but no docs yet, used make dist to create pause upload

=item 0.01.2
Added some documentation

=back

=head1 TODO

=over 8

=item 0.01

DOCUMENTATION DOCUMENTATION DOCUMENTATION DOCUMENTATION DOCUMENTATION !!!!!

=item 0.02

change the codes which represent the log_levels such that $xll style
log calls can have slightly different behavior in terms of being
programmer-debug calls, which will probably want to have detailed
prefix informtaion, while non-programmer-debug calls may want
different prefix info ($dll vs DEBUG)

=back


=head1 AUTHOR

Theo Lengyel, E<lt>theo@taowebs.net<gt>

=head1 SEE ALSO

L<perl>.

=cut
