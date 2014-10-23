# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

######################### We start with some black magic to print on failure.

# Change 1..1 below to 1..last_test_to_print .
# (It may become useful if the test is moved to ./t subdirectory.)

BEGIN { $| = 1; print "1..1\n"; }
END {print "not ok 1\n" unless $loaded;}
use WWW::Babelfish;
$loaded = 1;
print "ok 1\n";

######################### End of black magic.

# Insert your test code below (better if it prints "ok 13"
# (correspondingly "not ok 13") depending on the success of chunk 13
# of the test code):

print "Your WWW proxy (hostname:port): [none] ";
$proxy = <stdin>;

$obj = $proxy ? new WWW::Babelfish('proxy' => $proxy) : new WWW::Babelfish;
die( "Babelfish server unavailable\n" ) unless defined($obj);

print "Text to translate: ";
$text = <stdin>;

print "Source language (English, French, German, Italian, Spanish): ";
chomp($source = <stdin>);
print "Target language: ";
chomp($target = <stdin>);

print "Translating...\n";

$trans = $obj->translate( 'source' => $source,
			  'destination' => $target,
			  'text' => $text );

die("Could not translate: " . $obj->error) unless defined($trans);

print "Translation: ", $trans, "\n";
