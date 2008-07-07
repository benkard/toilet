/* -*- mode: objc; coding: utf-8 -*- */
/* Étoilisp/Mulklisp, a Common Lisp subset for the Étoilé runtime.
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

#import "MLKStringOutputStream.h"
#import "runtime-compatibility.h"

#import <Foundation/NSData.h>
#import <Foundation/NSString.h>
#import <Foundation/NSStream.h>


@implementation MLKStringOutputStream
-(id) init
{
  self = (id)[super initWithInputStream:nil
                    outputStream:AUTORELEASE ([[NSOutputStream alloc] initToMemory])
                    encoding:NSUnicodeStringEncoding];
  return self;
}

-(NSString *) string
{
  NSData *data = [_output propertyForKey:NSStreamDataWrittenToMemoryStreamKey];
  return AUTORELEASE ([[NSString alloc] initWithData:data
                                        encoding:NSUnicodeStringEncoding]);
}
@end
