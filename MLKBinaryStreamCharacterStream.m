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

#import "MLKBinaryStreamCharacterStream.h"
#import "runtime-compatibility.h"
#import "util.h"

#import <Foundation/NSException.h>

#include <unistd.h>
#include <stdlib.h>
#include <string.h>


@implementation MLKBinaryStreamCharacterStream
-(id) initWithBinaryStream:(MLKBinaryStream *)binaryStream
{
  return [self initWithBinaryStream:binaryStream
               encoding:NSUTF8StringEncoding];
}

-(id) initWithBinaryStream:(MLKBinaryStream *)binaryStream
                  encoding:(NSStringEncoding)encoding
{
  self = [super init];
  LASSIGN (_binaryStream, binaryStream);
  _encoding = encoding;
  return self;
}

-(unichar) readCharNoCache
{
  uint8_t *buffer;
  size_t i;
  unichar retval;

  buffer = NULL;
  for (i = 0;; i++)
    {
      NSString *tmpstr;

      buffer = (uint8_t *) realloc (buffer, i+1);
      buffer[i] = [_binaryStream readOctet];

      //NSLog (@"%@", _binaryStream);
      //NSLog(@"Read: 0x%x (%C)", buffer[i], buffer[i]);

      tmpstr = [[NSString alloc] initWithBytes:buffer
                                 length:(i+1)
                                 encoding:_encoding];
      if ([tmpstr length] == 1)
        {
          retval = [tmpstr characterAtIndex:0];
          [tmpstr release];
          //NSLog(@"Finished reading char: 0x%x (%C)", retval, retval);
          //FIXME: ? free (butval);
          return retval;
        }
      else
        {
          [tmpstr release];
        }
    }

  return -1;
}

-(void) writeChar:(unichar)ch
{
  const void *cstring = [[NSString stringWithFormat:@"%C", ch] cStringUsingEncoding:_encoding];
  const char *c;

  for (c = cstring; *c; c++)
    {
      [_binaryStream writeOctet:*c];
    }
}
@end
