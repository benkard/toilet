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

#import "MLKStreamStream.h"
#import "runtime-compatibility.h"
#import "util.h"

#import <Foundation/NSException.h>

#include <unistd.h>
#include <stdlib.h>
#include <string.h>


@implementation MLKStreamStream
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
  self = [super init];
  LASSIGN (_input, input);
  LASSIGN (_output, output);
  _closeInputWhenDone = NO;
  _closeOutputWhenDone = NO;
  return self;
}

-(uint8_t) readOctet
{
  uint8_t octet;
  size_t bytes_read;

  if ([_input streamStatus] == NSStreamStatusNotOpen)
    {
      _closeInputWhenDone = YES;
      [_input open];
    }

  bytes_read = [_input read:&octet maxLength:1];

  if (bytes_read < 1)
    {
      [NSException raise:@"MLKStreamError"
                   format:@"Tried to read beyond end of file."];
    }

  return octet;
}

-(void) writeOctet:(uint8_t)octet
{
  if ([_output streamStatus] == NSStreamStatusNotOpen)
    {
      _closeOutputWhenDone = YES;
      [_output open];
    }

  [_output write:&octet maxLength:1];
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
