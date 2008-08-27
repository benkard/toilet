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

#import "MLKStream.h"
#import "runtime-compatibility.h"
#import "util.h"

#import <Foundation/NSException.h>

#include <unistd.h>
#include <stdlib.h>
#include <string.h>


@implementation MLKStream
-(id) init;
{
  return [self initWithInputStream:nil outputStream:nil];
}

-(id) initWithInputStream:(NSInputStream *)input;
{
  return [self initWithInputStream:input outputStream:nil];
}

-(id) initWithOutputStream:(NSOutputStream *)output;
{
  return [self initWithInputStream:nil outputStream:output];
}

-(id) initWithInputStream:(NSInputStream *)input
             outputStream:(NSOutputStream *)output
{
  return [self initWithInputStream:input
               outputStream:output
               encoding:NSUTF8StringEncoding];
}


-(id) initWithInputStream:(NSInputStream *)input
             outputStream:(NSOutputStream *)output
                 encoding:(NSStringEncoding)encoding
{
  self = [super init];
  LASSIGN (_input, input);
  LASSIGN (_output, output);
  _encoding = encoding;
  _cachedChar = 0;
  _charCached = NO;
  _closeInputWhenDone = NO;
  _closeOutputWhenDone = NO;
  return self;
}

-(unichar) readChar
{
  uint8_t *buffer;
  size_t i;
  unichar retval;

  if (_charCached)
    {
      char ch;
      ch = _cachedChar;
      _cachedChar = 0;
      _charCached = NO;
      return ch;
    }

  if ([_input streamStatus] == NSStreamStatusNotOpen)
    {
      _closeInputWhenDone = YES;
      [_input open];
    }

  buffer = NULL;
  for (i = 0;; i++)
    {
      NSString *tmpstr;
      ssize_t bytes_read;

      //NSLog (@"%@", _input);

      buffer = (uint8_t *) realloc (buffer, i+1);
      bytes_read = [_input read:(buffer+i) maxLength:1];
      //NSLog (@"%d bytes read", bytes_read);

      if (bytes_read < 1)
        {
          free (buffer);
          [NSException raise:@"MLKStreamError"
                       format:@"Tried to read beyond end of file."];
        }

      tmpstr = [[NSString alloc] initWithBytes:buffer
                                 length:(i+1)
                                 encoding:_encoding];
      if ([tmpstr length] == 1)
        {
          retval = [tmpstr characterAtIndex:0];
          [tmpstr release];
          //free (buffer);
          return retval;
        }
      else
        {
          [tmpstr release];
        }
    }

  return -1;
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

  NS_DURING
    {
      [self peekChar];
    }
  NS_HANDLER
    {
      if ([[localException name] isEqual:@"MLKStreamError"])
        eofp = YES;
      else
        [localException raise];
    }
  NS_ENDHANDLER;

  return eofp;
}

-(void) writeChar:(unichar)ch
{
  const void *cstring = [[NSString stringWithFormat:@"%C", ch] cStringUsingEncoding:_encoding];
  [_output write:cstring maxLength:strlen(cstring)];
}

//-(void) writeFormat:(NSString *)format, ...
//{
//  NSString *string = ;
//  [self writeString:string];
//}

-(void) writeString:(NSString *)string
{
  unichar ch;
  int i;
  
  for (i = 0; i < [string length]; i++)
    [self writeChar:[string characterAtIndex:i]];
}

-(void) dealloc
{
  if (_closeInputWhenDone)
    {
      [_input close];
    }
  LRELEASE (_input);
  if (_closeOutputWhenDone)
    {
      [_output close];
    }
  LRELEASE (_output);
  [super dealloc];
}
@end
