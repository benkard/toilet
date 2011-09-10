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
#import "MLKStringInputStream.h"
#import "runtime-compatibility.h"
#import "util.h"

#import <Foundation/NSString.h>
#import <Foundation/NSStream.h>


@implementation MLKStringInputStream
-(MLKStringInputStream *) init
{
  return [self initWithString:@""];
}

-(MLKStringInputStream *) initWithString:(NSString *)string
{
  // We used to use NSUnicodeStringEncoding here, but Mac OS X has
  // the strange habit of using a byte-order mark in internal string
  // representations.  This complicates matters: -readCharNoCache
  // will correctly read the first character of a string, since the
  // beginning of the string includes the BOM, but subsequent 
  // characters may or may not be read correctly depending on the 
  // host's default endianness.
  MLKStreamStream *binstream =
    LAUTORELEASE ([[MLKStreamStream alloc]
                    initWithInputStream:
                      [NSInputStream inputStreamWithData:
                                       [string dataUsingEncoding:
                                                 NSUTF8StringEncoding]]]);
  self = (id) [super initWithBinaryStream:binstream
                     encoding:NSUTF8StringEncoding];
  return self;
}

+(MLKStringInputStream *) streamWithString:(NSString *)string
{
  return LAUTORELEASE ([[self alloc] initWithString:string]);
}
@end
