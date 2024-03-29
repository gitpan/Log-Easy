use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
              AUTHOR            => 'Theo Lengyel <dirt@cpan.org>',
              NAME		=> 'Log::Easy',
              VERSION_FROM	=> 'Easy.pm',
              ABSTRACT_FROM     => 'Easy.pm',
              PREREQ_PM		=> { Filter::Simple => '.01',
                                   },
              PM                => { 'Easy.pm'   => '$(INST_LIBDIR)/Easy.pm',
                                     'Filter.pm' => '$(INST_LIBDIR)/Easy/Filter.pm',
                                     'Apache.pm' => '$(INSTALLSITELIB)/Apache/Log/Easy.pm',
                                   },
             );
