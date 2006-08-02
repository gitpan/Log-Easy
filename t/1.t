use strict;
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 1.t'
#print STDOUT 'LINE:', __LINE__, "\n";
#########################

# change 'tests => 1' to 'tests => last_test_to_print';

#use Test::More qw( no_plan );
use Test::More;
BEGIN {
  $ENV{BIG_WARN_ON_CRIT} = 0;
  $ENV{BIG_WARN_ON_ERROR} = 0;
  $ENV{BIG_WARN_ON_FATAL} = 0;
}
#BEGIN { use_ok('Log::Easy') };

#########################

# Insert your test code below, the Test::More module is use()ed here so read
# its man page ( perldoc Test::More ) for help writing this test script.

use Log::Easy qw(:all);
$log->log_file('STDERR');
my $test_log = new Log::Easy;
$test_log->handle_fatals(0);
$test_log->prefix('');
$test_log->{DEBUG}{NO_QUIT} = 1;
$test_log->{DEBUG}{NO_ALERT} = 1;
my $log_codes  = { $test_log->LOG_CODE() };
my $always_log = { $test_log->ALWAYS_LOG() };
#$log->write(LOUD, '$always_log: ', $always_log );
#die;
my @log_levels = $log->LOG_LEVELS();
#$log->write($lll, '@log_levels: ', \@log_levels );

plan tests => (scalar @log_levels)**2;

$test_log->log_file("/dev/null") unless $ENV{TEST_SHOW_LOG_OUTPUT};
#$test_log->prefix('');
#$test_log->{EVIL} = 1;
my ( $msg, $status );

$log->write(CLEAN, "");
my ( $testcnt, $maxtestcnt ) = ( 0, -6 );

foreach my $set_level ( @log_levels ) {
  $log->write($dll, '$set_level: ', $set_level );
  $test_log->log_level( $set_level );
  foreach my $try_level ( @log_levels ) { 
    $log->write($dll, '$try_level: ', $try_level );
    $testcnt++;
    my $set_try = "$set_level:$try_level";
    #$log->write($lll, '$set_try: ', $set_try );
    my ( $xstatus, $xmsg ) = $log->write({n=>'',log_file => '/dev/null'}, CLEAN, " ...   NEXT ATTEMPT::: ", $set_try);
    {
      #local $ENV{LOG_INTERNAL_DEBUG} = 5;
      ( $status, $msg ) = $test_log->write({log_file => '/dev/null'}, $try_level, " ...   ACTUAL ATTEMPT::: ", $set_try);
    }
    my $map_set_level = $set_level;
    my $map_try_level = $try_level;
    $map_set_level =~ s/^D_//;
    $map_try_level =~ s/^D_//;
    my ( $_set_level, $_try_level ) = ( $log_codes->{$map_set_level}, $log_codes->{$map_try_level} );
    my $expected = ( $_set_level <= $_try_level ) ? 1 : $always_log->{$map_try_level} ? 1 : 0;
    $msg =~ s/\n//mg;
    $log->write($lll, '$_set_level: ', $testcnt, ": ", $_set_level, "=$set_level" );
    $log->write($lll, '$_try_level: ', $testcnt, ": ", $_try_level, "=$try_level" );
    my $ismsg = "$xmsg $msg (x=$xstatus:s=$status#e=$expected)";
    #print STDERR "$ismsg\n";
    if ( not is( $status, $expected, $ismsg ) ) {
      ( $status, $msg ) = $test_log->write({log_file => '/dev/tty'}, LOUD, "\n---\n FAILED ...   ACTUAL ATTEMPT::: ", $set_try, " ::: $ismsg \n---");
      ( $status, $msg ) = $log->write({log_file => '/dev/tty'}, $try_level, "\n---\n FAILED ...   ACTUAL ATTEMPT::: ", $set_try, " ::: $ismsg \n---", $test_log);
      diag($ismsg);#"$set_try\n" );
      die;
    }
    exit 0 if ( $maxtestcnt > 0 and $testcnt > $maxtestcnt );
    #    if (not ($expected == $status)) {
    #      $log->write(CLEAN, "TRYING: '$set_try' => '$_set_level:$_try_level'");
    #      $log->write(CLEAN, "GOT: '$msg' -> STATUS: $status, EXPECTED: '$expected'");
    #      $log->write(CLEAN, " ...");
    #    }
  }
}
