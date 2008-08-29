###############################################################################
#
#   Class: NaturalDocs::Languages::ObjC
#
###############################################################################
#
#   Handle Objective-C.
#
###############################################################################

## Copyright 2008, Matthias Andreas Benkard.
##
## This package is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This package is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this package; if not, write to the Free Software
## Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA

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

  $_ = $prototype;
  if ($topic_type == ::TOPIC_FUNCTION and /([-+]\s*\((.*?)\)\s*)(.*)/)
  {
    my $return_type = $2;
    my $p = NaturalDocs::Languages::Prototype->New ($1, "");
    my $args_p = 0;

    $_ = $3;
    while (/(\S+)\((.*?)\)\s*(\S+)(?:(\s+(.*))?|$)/)
    {
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
