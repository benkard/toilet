###############################################################################
#
#   Class: NaturalDocs::Languages::ObjC
#
###############################################################################
#
#   Handle Objective-C.
#
###############################################################################

use strict;
use integer;

package NaturalDocs::Languages::ObjC;

use base 'NaturalDocs::Languages::Simple';
use NaturalDocs::Languages;

sub OnCode  
{
  my ($self, $codeLines, $codeLineNumber, $topicList, $lastCommentTopicCount) = @_;
  $self->SUPER::OnCode ($codeLines, $codeLineNumber, $topicList, $lastCommentTopicCount);

  my $topic = $topicList->[-1];
  my $line = $codeLines->[0];
  if ($lastCommentTopicCount)
  {
    #print "$topic\n";
    #print "$line\n";
    unless ($topic->Prototype())
    {
      my $code = join ("\n", @{$codeLines});
      if ($topic->Type() eq ::TOPIC_FUNCTION())
      {
        if ($code =~ /\s*([-+].*)[;{]/)
        {
          $topic->SetPrototype($1);
        }
      }
      elsif ($topic->Type() eq ::TOPIC_CLASS())
      {
        if ($code =~ /\s*(\@interface.*\n)/)
        {
          $topic->SetPrototype($1);
        }
      }
    }
  }
};

sub ParsePrototype
{
  my ($self, $topic_type, $prototype) = @_;

  #print "$prototype\n";

  $_ = $prototype;
  if ($topic_type == ::TOPIC_FUNCTION and /([-+]\s*\((.*?)\)\s*)(.*)/)
  {
    my $return_type = $2;
    my $p = NaturalDocs::Languages::Prototype->New ($1, "");
    my $args_p = 0;

    $_ = $3;
    while (/(\S+)\((.*?)\)\s*(\S+)(?:(\s+(.*))?|$)/)
    {
      #print "$4, $1, $2, $3\n";
      $p->AddParameter (NaturalDocs::Languages::Prototype::Parameter->New ("($2)",
                                                                           $1,
                                                                           $3,
                                                                           "",
                                                                           "",
                                                                           ""));
      $args_p = 1;
      $_ = $4;
    }

    if ($args_p)
    {
      return $p;
    }
    else
    {
      return NaturalDocs::Languages::Prototype->New ($prototype, "");
    }
  }
  else
  {
    return $self->SUPER::ParsePrototype ($topic_type, $prototype);
  }
};

1;
