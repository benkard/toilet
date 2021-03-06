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

#import "MLKBinaryStream.h"

#import <Foundation/NSStream.h>

@interface MLKStreamStream : MLKBinaryStream
{
  NSInputStream *_input;
  NSOutputStream *_output;
  BOOL _closeInputWhenDone;
  BOOL _closeOutputWhenDone;
}

-(id) init;
-(id) initWithInputStream:(NSInputStream *)input;
-(id) initWithOutputStream:(NSOutputStream *)output;
-(id) initWithInputStream:(NSInputStream *)input
             outputStream:(NSOutputStream *)output;

-(uint8_t) readOctet;
-(void) writeOctet:(uint8_t)octet;
@end
