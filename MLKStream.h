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

#import "MLKLispValue.h"
#import <Foundation/Foundation.h>

@class NSInputStream, NSOutputStream;


@interface MLKStream : MLKLispValue
{
  NSInputStream *_input;
  NSOutputStream *_output;
  NSStringEncoding _encoding;
  BOOL _charCached;
  unichar _cachedChar;
}

-(MLKStream *) init;
-(MLKStream *) initWithInputStream:(NSInputStream *)input;
-(MLKStream *) initWithOutputStream:(NSOutputStream *)output;
-(MLKStream *) initWithInputStream:(NSInputStream *)input
                      outputStream:(NSOutputStream *)output;
-(MLKStream *) initWithInputStream:(NSInputStream *)input
                      outputStream:(NSOutputStream *)output
                          encoding:(NSStringEncoding)encoding;

-(unichar) readChar;
-(void) unreadChar:(unichar)ch;
-(unichar) peekChar;
-(BOOL) isEOF;
@end
