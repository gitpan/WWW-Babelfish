use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'	=> 'WWW::Babelfish',
    'VERSION_FROM' => 'Babelfish.pm', # finds $VERSION
    'PREREQ_PM' => {

		    'HTTP::Request' => 0,
		    'LWP::UserAgent' => 0,
		    'HTML::TokeParser' => 0,
		    'IO::String' => 0,
		   }
);
