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

$VERSION = '0.11';

# Preloaded methods go here.

use HTTP::Request::Common qw(POST);
use LWP::UserAgent;
use HTML::TokeParser;
use IO::String;

my $BABELFISH = 'babelfish.altavista.com';
my $BABELFISH_URL = 'http://babelfish.altavista.com/babelfish/tr';
my $MAXCHUNK = 1000; # Maximum number of characters 
# Bablefish will translate at one time 

my $MAXRETRIES = 5;  # Maximum number of retries for a chunk of text
$| = 1;

sub new {
    my ($this, @args) = @_;
    my $class = ref($this) || $this;
    my $self = {};
    bless $self, $class;
    return undef unless( $self->initialize(@args) );
    return $self;
}

sub initialize {
    my($self, %params) = @_;;

    # Caller can set user agent; we default to "script:WWW::Babelfish/0.01"
    $self->{agent} = $params{agent} || $0 . ":" . __PACKAGE__ . "/" . $VERSION;

    $self->{proxy} = $params{proxy} if defined $params{proxy};

    # Get the page 
    my $ua = new LWP::UserAgent;
    $ua->proxy('http','http://' . $self->{proxy}) if defined $self->{proxy};
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
  
  # Paragraph separator is "\n\n" by default
  $/ = $params{delimiter} || "\n\n";
  
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
  
  # This "feature" is actually useful as a pass-thru filter.
  # Babelfish doesn't do same-to-same anyway (though it would be
  # pretty interesting if it did)
  return $params{text} if $params{source} eq $params{destination};
  
  my $langopt = $self->{Lang2opt}->{$params{source} . " to " . $params{destination}};
  
  my $th;			# "Text Handle"
  if( ref $params{text} ){	# We've been passed a filehandle
    $th = $params{text};  
  }
  else{				# We've been passed a string
    $th = new IO::String($params{text});
  }
  
  my $Text = "";
  my $WANT_STRING_RETURNED = 0;
  unless( defined $params{ofh} ){
    $params{ofh} = new IO::String($Text);
    $WANT_STRING_RETURNED = 1;
  }
  
  # Variables we use in the next mega-block
  my $para;			# paragraph
  my $num_paras = 0;		# number of paragraphs
  my $transpara;		# translated paragraph
  my $para_start_ws = "";	# initial whitespace in paragraph
  my $chunk;			# paragraph piece to feed to babelfish
  my $req;			# LWP request object
  my $ua;			# LWP user agent
  my $res;			# LWP result
  my $text;			# translated chunk
  my $i;			# a counter
  while($para = <$th>){
    $num_paras++;
    $transpara = "";
    
    # Extract any leading whitespace from the start of the paragraph
    # Babelfish will eat it anyway.
    if ($para =~ s/(^\s+)(\S)/$2/) {
	$para_start_ws = $1 || "";
    }
    chomp $para;		# Remove the para delimiter
    
  CHUNK:
    foreach $chunk ( $self->_chunk_text($MAXCHUNK, $para) ) {
      $req = POST ($BABELFISH_URL, [ 'doit' => 'done', 'urltext' => $chunk, 'lp' => $langopt, 'Submit' => 'Translate', 'enc' => 'utf8' ]);
      $ua = new LWP::UserAgent;
      $ua->proxy('http','http://' . $self->{proxy}) if defined $self->{proxy};
      
    RETRY:
      for($i = 0; $i <= $MAXRETRIES; $i++){ 
	$res = $ua->request($req);
	if( $res->is_success ){

	  #DEBUG
	  open(DEBUG, ">debug.html");
	  print DEBUG $res->as_string;
	  close DEBUG;

	  $text = $self->_extract_text($res->as_string);
	  next RETRY if $text =~ /^\*\*time-out\*\*/; # in-band signalling; yuck
	  
	  $text =~ s/\n$//;	# Babelfish likes to append newlines
	  $transpara .= $text;
	  
	  next CHUNK;
	}
      }
      $self->{error} = "Request timed out more than $MAXRETRIES times";
      return undef; 
    }
    print { $params{ofh} } $/ if $num_paras > 1;
    print { $params{ofh} } $para_start_ws . $transpara;
  }
  
  close $params{ofh};
  if( $WANT_STRING_RETURNED ){
    return $Text;
  }
  else{
    return 1;
  }
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
	if( $offset == -1 or $offset <= $lastoffset   ){
	    @_ = sort {$b <=> $a} ( rindex($text, ',', $test), 
				    rindex($text, ';', $test),      
				    rindex($text, ':', $test),      
				    ); 
	    $offset = shift(@_) + 1;


	    # or by word
	    if( $offset == -1 or $offset <= $lastoffset){
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

# Extract the text from the html we get back from babelfish and return
# it (keying on the fact that it's the first thing after a <br> tag,
# possibly removing a textarea tag after it).
sub _extract_text {
  my($self, $html) = @_;

  my $p = HTML::TokeParser->new(\$html);

  my $tag;
  while ($tag = $p->get_tag('input')) {
    return @{$tag}[1]->{value} if @{$tag}[1]->{name} eq 'q';
  }

# This code is now obsoleted by the new result page format, but I'm
# leaving it here commented out in case we end up needing the
# whitespace hack again.
#
#    my ($tag,$token);
#    my $text="";
#     if ($tag = $p->get_tag('br')) {
# 	while ($token = $p->get_token) {
# 	    next if shift(@{$token}) ne "T";
# 	    $text = shift(@{$token});

# 	    #$text =~ s/[\r\n]//g;
# 	    # This patch for whitespace handling from Olivier Scherler
#             $text =~ s/[\r\n]/ /g;
#             $text =~ s/^\s*//;
#             $text =~ s/\s+/ /g;
#             $text =~ s/\s+$//;

# 	    last if defined($text) and $text ne "";
# 	}
#    }
#    return $text;


}


# Autoload methods go after =cut, and are processed by the autosplit program.

1;
__END__

=head1 NAME

WWW::Babelfish - Perl extension for translation via babelfish

=head1 SYNOPSIS

  use WWW::Babelfish;
  $obj = new WWW::Babelfish( 'agent' => 'Mozilla/8.0', 'proxy' => 'myproxy' );
  die( "Babelfish server unavailable\n" ) unless defined($obj);

  $french_text = $obj->translate( 'source' => 'English',
                                  'destination' => 'French',
                                  'text' => 'My hovercraft is full of eels',
				  'delimiter' => "\n\t",
				  'ofh' => \*STDOUT );
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

Translates some text using Babelfish.

Parameters: 

 source:      Source language
 destination: Destination language
 text:        If this is a reference, translate interprets it as an 
              open filehandle to read from. Otherwise, it is treated 
              as a string to translate.
 delimiter:   Paragraph delimiter for the text; the default is "\n\n".
              Note that this is a string, not a regexp.
 ofh:         Output filehandle; if provided, the translation will be 
              written to this filehandle.

If no ofh parameter is given, translate will return the text; otherwise 
it will return 1. On failure it returns undef.


=item error

Returns a (hopefully) meaningful error string.

=back

=head1 NOTES

Babelfish translates 1000 characters at a time. This module tries to
break the source text into reasonable logical chunks of less than 1000
characters, feeds them to Babelfish and then reassembles
them. Formatting may get lost in the process.

=head1 AUTHOR

Dan Urist, durist@world.std.com

=head1 SEE ALSO

perl(1).

=cut
