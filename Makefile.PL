use 5.008;
use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
              AUTHOR            => 'Theo Lengyel <dirt@cpan.org>',
              NAME		=> 'Log::Easy',
              VERSION_FROM	=> 'Log/Easy.pm',
              ABSTRACT_FROM     => 'Log/Easy.pm',
              PREREQ_PM		=> { Filter::Simple => '.01',
                                   },
             );
