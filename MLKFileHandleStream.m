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

#import "MLKFileHandleStream.h"
#import "runtime-compatibility.h"
#import "util.h"

#import <Foundation/NSData.h>
#import <Foundation/NSException.h>

#include <unistd.h>
#include <stdlib.h>
#include <string.h>


@implementation MLKFileHandleStream
-(id) initWithFileHandle:(NSFileHandle *)fileHandle
{
  return [self initWithFileHandle:fileHandle
               closeWhenDone:NO];
}

-(id) initWithFileHandle:(NSFileHandle *)fileHandle
           closeWhenDone:(BOOL)closeWhenDone
{
  self = [super init];
  LASSIGN (_fileHandle, fileHandle);
  _closeWhenDone = closeWhenDone;
  return self;
}

-(uint8_t) readOctet
{
  NSData *data;

  data = [_fileHandle readDataOfLength:1];

  if ([data length] == 0)
    {
      [NSException raise:@"MLKStreamError"
                   format:@"Tried to read beyond end of file."];
    }

  return *(uint8_t*)[data bytes];
}

-(void) writeOctet:(uint8_t)octet
{
  [_fileHandle writeData:[NSData dataWithBytesNoCopy:&octet
                                 length:1
                                 freeWhenDone:NO]];
}

-(void) dealloc
{
  if (_closeWhenDone)
    {
      [_fileHandle closeFile];
    }
  LDESTROY (_fileHandle);
  [super dealloc];
}
@end
