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

#include "MLKReadtable.h"
#include "MLKStream.h"

#include <Foundation/NSObject.h>
#include <Foundation/NSString.h>


@interface MLKReader : NSObject
+(id)    readFromStream:(MLKStream *)stream
               eofError:(BOOL)eofError
               eofValue:(id)eofValue
              recursive:(BOOL)recursive
     preserveWhitespace:(BOOL)preserveWhitespace
        singleDotMarker:(id)dotMarker
readingUninternedSymbol:(BOOL)readingUninternedSymbol;

+(id) readFromStream:(MLKStream *)stream
            eofError:(BOOL)eofError
            eofValue:(id)eofValue
           recursive:(BOOL)recursive
  preserveWhitespace:(BOOL)preserveWhitespace;

+(id) readFromString:(NSString *)string;

+(id) interpretToken:(NSString *)token
           readtable:(MLKReadtable *)readtable
             escaped:(BOOL)escaped;

+(BOOL) isPotentialNumber:(NSString *)token
                readtable:(MLKReadtable *)table
                     base:(int)base;
@end
