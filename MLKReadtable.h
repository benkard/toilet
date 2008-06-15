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

#import <Foundation/NSObject.h>
#import <Foundation/NSString.h>

@class MLKClosure, NSMutableDictionary;


enum MLKReadtableCase
{
  MLKReadtableCase_UPCASE,
  MLKReadtableCase_DOWNCASE,
  MLKReadtableCase_INVERT,
  MLKReadtableCase_PRESERVE
};


@interface MLKReadtable : MLKLispValue <NSCopying>
{
  NSMutableDictionary *_syntaxTable;
  NSMutableDictionary *_readerMacros;
  //MLKClosure *_caseConverter;
  enum MLKReadtableCase _case;
}

-(MLKReadtable *) init;

-(MLKReadtable *) copyWithZone:(NSZone *)zone;

-(BOOL) isWhitespaceCharacter:(unichar)ch;
-(BOOL) isMacroCharacter:(unichar)ch;
-(BOOL) isNonTerminatingMacroCharacter:(unichar)ch;
-(BOOL) isTerminatingMacroCharacter:(unichar)ch;
-(BOOL) isSingleEscapeCharacter:(unichar)ch;
-(BOOL) isMultipleEscapeCharacter:(unichar)ch;
-(BOOL) isConstituentCharacter:(unichar)ch;
-(BOOL) characterHasCase:(unichar)ch;

// Constituent traits.
-(BOOL) isInvalid:(unichar)ch;
-(BOOL) isAlphabetic:(unichar)ch;
-(BOOL) isPackageMarker:(unichar)ch;
-(BOOL) isAlphaDigit:(unichar)ch;
-(BOOL) isExponentMarker:(unichar)ch;
-(BOOL) isNumberMarker:(unichar)ch;
-(BOOL) isRatioMarker:(unichar)ch;
-(BOOL) isDecimalPoint:(unichar)ch;
-(BOOL) isMinusSign:(unichar)ch;
-(BOOL) isPlusSign:(unichar)ch;
-(BOOL) isSign:(unichar)ch;
-(BOOL) isDot:(unichar)ch;

// Read-base-dependent digit properties.
-(BOOL) isDigit:(unichar)ch;
-(int) digitWeight:(unichar)ch;

-(MLKClosure *) macroFunctionForCharacter:(unichar)ch;
-(unichar) charWithReadtableCase:(unichar)ch;
@end
