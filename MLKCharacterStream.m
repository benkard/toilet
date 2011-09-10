/* -*- mode: objc; coding: utf-8 -*- */
/* Toilet Lisp, a Common Lisp subset for the Étoilé runtime.
 * Copyright (C) 2008  Matthias Andreas Benkard.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at
 * your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#import "MLKCharacterStream.h"
#import "MLKStreamError.h"
#import "runtime-compatibility.h"
#import "util.h"

#import <Foundation/NSException.h>

#include <unistd.h>
#include <stdlib.h>
#include <string.h>


@implementation MLKCharacterStream
-(id) init;
{
  self = [super init];
  _cachedChar = 0;
  _charCached = NO;
  return self;
}

-(unichar) readChar
{
  if (_charCached)
    {
      unichar ch;
      ch = _cachedChar;
      _cachedChar = 0;
      _charCached = NO;
      return ch;
    }
  else
    {
      return [self readCharNoCache];
    }
}

-(void) unreadChar:(unichar)ch
{
  if (_charCached)
    [NSException raise:@"MLKInvalidOperationError"
                 format:@"Attempted to UNREAD-CHAR twice in a row."];

  _charCached = YES;
  _cachedChar = ch;
}

-(unichar) peekChar
{
  unichar ch = [self readChar];
  [self unreadChar:ch];
  return ch;
}

-(BOOL) isEOF
{
  BOOL eofp = NO;

  @try
    {
      [self peekChar];
    }
  @catch (MLKStreamError *e)
    {
      eofp = YES;
    }

  return eofp;
}

-(void) writeString:(NSString *)string
{
  int i;
  
  for (i = 0; i < [string length]; i++)
    [self writeChar:[string characterAtIndex:i]];
}

-(void) writeChar:(unichar)ch
{
  [NSException raise:@"MLKNotImplementedError" format:@""];
  ch = 0;
}

-(unichar) readCharNoCache
{
  [NSException raise:@"MLKNotImplementedError" format:@""];
  return 0;
}
@end
