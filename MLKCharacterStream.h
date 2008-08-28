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

#import <Foundation/NSStream.h>
#import <Foundation/NSString.h>


@interface MLKCharacterStream : NSObject
{
  BOOL _charCached;
  unichar _cachedChar;
}

-(id) init;

// To implement by subclasses:
-(unichar) readCharNoCache;
-(void) writeChar:(unichar)ch;

-(unichar) readChar;
-(void) unreadChar:(unichar)ch;
-(unichar) peekChar;
-(BOOL) isEOF;

//-(void) writeFormat:(NSString *)format, ...;
-(void) writeString:(NSString *)string;
@end
