package Apache::Log::Easy;
use Log::Easy qw(:all);
use Log::Easy::Filter;
my $filter_file;
our $this_package;
BEGIN {
  # this little but of cruft really sucks, but neither 'require' nor 'do' are bahaving as I would expect(akin to a c #include) 
  # require '/home/lengthe/cvs/adg/util/general/Log/Filter.pm';
  #require Log::Easy::Filter;
  #do Log::Easy::Filter;
  $filter_file = ( __PACKAGE__ eq 'Log::Easy' ? __FILE__ : $INC{'Log/Easy.pm'} ) or die "Couldn't find location of Log/Easy.pm package";
  $filter_file =~ s|Easy.pm|Easy/Filter.pm|;
  #print STDERR "filter_file=$filter_file\n";
  my $eval = 'package '  . __PACKAGE__ . ';';
  # this is somewhat evil, but I need to do it to get filtering in THIS package, as well as packages that use this package
  $eval .= '#' . `cat $filter_file`; # the '#' here comments out the first line of the filter package 'package Log::Easy::Filter;'
  print STDERR $eval if $ENV{LOG_FILTER_DEBUG};
  eval "{ $eval }";
  $@ and die $@;
  #die;
}
use strict;
our @ISA = qw( Log::Easy );
use Exporter;
our %EXPORT_TAGS = %Log::Easy::EXPORT_TAGS;
our @EXPORT_OK   = @{$EXPORT_TAGS{'all'}};
our @EXPORT      = ();
our $log = new Apache::Log::Easy;
our $plog = new Log::Easy;
$plog->log_file('STDERR');
sub write {
  #die;
  my $self = shift;
  my @msg = @_;
  my ($level, $_level, $log_level, $_log_level, $args)  = $self->_check_level( \@msg );
  $plog->write(DEBUG, '$args: ', $args );
  my $n = exists $args->{n} ? $args->{n} : ($self->{n} || "\n");
  my ( $status, $return);
  { 
    local $args->{dont_actually_log} = 1;
    local $args->{backstack} = 1;
    $plog->write($dll, '$args: ', $args );
    ( $status, $return ) = $self->SUPER::write($args, $level, @msg );
    #$plog->write(DEBUG, '$status: ', $status );
    #$plog->write(DEBUG, '$return: ', $return );
  }
  if ( $status ) {
    $return .= $n;
    if ( $ENV{GATEWAY_INTERFACE} and $args->{log_file} eq 'STDOUT' ) {
      $return =~ s/\n/<BR>\n/mg;
      my $s = '&nbsp;';
      my $t = $s x 5;
      $return =~ s/\ /$s/mg;
      $return =~ s/\t/$t/mg;
    }
    my $fh = $self->fh( %$args, -level => $level, -message => $return );
    unless ( $args->{dont_actually_log} ) {
      print $fh $return;
    }
  }
  #$plog->write($lll, '$status: ', $status );
  #$plog->write($lll, '$return: ', $return );
  return wantarray ? ( $status, $return ) : $status ;
}
# sub _actually_log {
# # this is where we would probably normally pass this log message off to apache unless the log_file is STDOUT
# # then again maybe not :-)
#} 
1;
