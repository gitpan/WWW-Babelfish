package WWW::Babelfish;

use strict;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK);

require Exporter;
require AutoLoader;

@ISA = qw(Exporter AutoLoader);
# Items to export into callers namespace by default. Note: do not export
# names by default without a very good reason. Use EXPORT_OK instead.
# Do not simply export all your public functions/methods/constants.
@EXPORT = qw();

$VERSION = '0.02';

# Preloaded methods go here.

use HTTP::Request::Common qw(POST);
use LWP::UserAgent;
use HTML::TokeParser;

my $BABELFISH = 'babelfish.altavista.digital.com';
my $BABELFISH_URL = 'http://babelfish.altavista.digital.com/cgi-bin/translate?';
my $MAXCHUNK = 1000; # experimentally determined maximum number of characters 
                     # Bablefish will translate at one time 

my $MAXRETRIES = 5;  # Maximum number of retries for a chunk of text


sub new {
  my ($this, @args) = @_;
  my $class = ref($this) || $this;
  my $self = {};
  bless $self, $class;
  return undef unless( $self->initialize(@args) );
  return $self;
}

sub initialize {
  my($self, %params) = shift;

  # Caller can set user agent; we default to "script:WWW::Babelfish/0.01"
  $self->{agent} = $params{agent} || $0 . ":" . __PACKAGE__ . "/" . $VERSION;

  # Get the page 
  my $ua = new LWP::UserAgent;
  $ua->agent($self->{agent});     
  my $req = new HTTP::Request('GET' => $BABELFISH_URL);
  my $res = $ua->request($req);
  unless($res->is_success){ 
    warn(__PACKAGE__ . ":" . $res->status_line);
    return 0;
  }
  my $page = $res->content;

  # Extract the mapping of languages to options to be passed back,
  # and store it on our object in "Lang2opt" hash
  # and extract language names and store on "Langs" hash 
  my $p = HTML::TokeParser->new(\$page);
  my $a2b;
  if( $p->get_tag("select") ){
    while( $_ = $p->get_tag("option") ){
      $a2b = $p->get_trimmed_text;
      $self->{Lang2opt}->{$a2b} = $_->[1]{value};
      $a2b =~ /(\S+)\sto\s(\S+)/;
      $self->{Langs}->{$1} = ""; 
      $self->{Langs}->{$2} = ""; 
    }
  }

  return 1;
}

sub languages {
  my $self = shift;
  return sort keys %{$self->{Langs}};
}

sub translate {
  my ($self, %params) = @_;

  undef $self->{error};
  unless( exists($self->{Langs}->{$params{source}}) ){
    $_ = "Language \"" . $params{source} . "\" is not available";
    $self->{error} = $_;
    warn(__PACKAGE__ . ": " . $_);
    return undef;
  }

  unless( exists($self->{Langs}->{$params{destination}}) ){
    $_ =  "Language \"" . $params{destination} . "\" is not available";
    $self->{error} = $_;
    warn(__PACKAGE__ . ": " . $_);
    return undef;
  }

  # This "feature" is actually useful as a pass-thru filter
  return $params{text} if $params{source} eq $params{destination};

  my $langopt = $self->{Lang2opt}->{$params{source} . " to " . $params{destination}};

  my $Text;
  my ($para, $delim, $chunk, $req, $res, $ua, $answer, $p, $tag, $text, $i);

  my @paras =  split(/(\n{2,}|\n\s+)/, $params{text});
  foreach $para (@paras) {
    my $delim = shift(@paras);

  CHUNK:
    foreach $chunk ( $self->_chunk_text($MAXCHUNK, $para) ) {
      $req = POST ($BABELFISH_URL, [ 'doit' => 'done', 'urltext' => $chunk, 'lp' => $langopt, 'Submit' => 'Translate' ]);
      $ua = new LWP::UserAgent;

      for($i = 0; $i <= $MAXRETRIES; $i++){ 
	$res = $ua->request($req);
	if($res->is_success){
	  # Now parse out the translated text (keying on the fact 
	  # that it's the first thing aligned left)
	  $answer = $res->as_string;

	  $p = HTML::TokeParser->new(\$answer);
	  while( $tag = $p->get_tag('td') ){
	    $_ = pop(@{$tag});
	    if($_ eq '<td align="left">'){
	      $tag = $p->get_tag('font');
	      $text = $p->get_text;
	      last;
	    }
	  }
	  $Text .= $text;
	  next CHUNK;
	}
      }
      $self->{error} = "Request timed out more than $MAXRETRIES times";
      return undef; # We return all or nothing
    }
    $Text .= $delim;
  }
  return $Text || undef;
}

sub error {
  my $self = shift;
  return $self->{error};
}

# Given a maximum chunk size and some text, return 
# an array of pieces of the text chopped up in a 
# logical way and less than or equal to the chunk size
sub _chunk_text {
  my($self, $max, $text) = @_;
  
  my @result;
  
  # The trivial case
  return($text) if length($text) <= $max; 

  # Hmmm. There are a couple of ways we could do this. 
  # I'm guessing that Babelfish doesn't look at any structure larger than 
  # a sentence; in fact I'm often tempted to guess that it doesn't look
  # at anything larger than a word, but we'll give it the benefit of the doubt.

  my $total = length($text);
  my $offset = 0;
  my $lastoffset = 0;
  my $test;
  my $chunk;

  while( ($total - $lastoffset) > $max) {
    $test = $lastoffset + $max;
    
    # Split by terminal punctuation...
    @_ = sort {$b <=> $a} ( rindex($text, '.', $test), 
			    rindex($text, '!', $test),      
			    rindex($text, '?', $test),      
			  );
    $offset = shift(@_) + 1;

    # or by clause...
    if( $offset == -1 || $offset <= $lastoffset   ){
      @_ = sort {$b <=> $a} ( rindex($text, ',', $test), 
			      rindex($text, ';', $test),      
			      rindex($text, ':', $test),      
			    ); 
      $offset = shift(@_) + 1;


      # or by word
      if( $offset == -1 || $offset <= $lastoffset){
	$offset = rindex($text, " ", $test);
      }
      
      # or give up
      return undef if $offset == -1;
    }
  
    $chunk = substr($text, $lastoffset, $offset - $lastoffset);

    push( @result, $chunk);
    $lastoffset = $offset;
  }

  push( @result, substr($text, $lastoffset) );
  return @result;
}


# Autoload methods go after =cut, and are processed by the autosplit program.

1;
__END__

=head1 NAME

WWW::Babelfish - Perl extension for translation via babelfish

=head1 SYNOPSIS

  use WWW::Babelfish;
  $obj = new WWW::Babelfish( 'agent' => 'Mozilla/8.0' );
  die( "Babelfish server unavailable\n" ) unless defined($obj);

  $french_text = $obj->translate( 'source' => 'English',
                                  'destination' => 'French',
                                  'text' => 'My hovercraft is full of eels');
  die("Could not translate: " . $obj->error) unless defined($french_text);

  @languages = $obj->languages;

=head1 DESCRIPTION

Perl interface to the WWW babelfish translation server.

=head1 METHODS

=over 4

=item new

Creates a new WWW::Babelfish object. It can take a named argument for
user agent.

=item languages

Returns a plain array of the languages available for translation.

=item translate

Takes named arguments for a source language, a destination language,
and a source text. Returns translated text.

=item error

Returns a (hopefully) meaningful error string.

=back

=head1 NOTES

Babelfish only seems to translate about 1000 characters at a
time. This module tries to break the source text into reasonable
logical chunks of less than 1000 characters, feeds them to Babelfish
and then reassembles them. Any formatting is likely to get lost in the
process.

=head1 CAVEATS

The translate function is an all-or-nothing proposition; if we time out
too many times or can't connect to Babelfish, no partial translation is 
returned.

=head1 AUTHOR

Dan Urist, durist@world.std.com

=head1 SEE ALSO

perl(1).

=cut
