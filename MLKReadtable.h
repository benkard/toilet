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

#import "MLKFuncallable.h"

#import <Foundation/NSDictionary.h>
#import <Foundation/NSObject.h>
#import <Foundation/NSString.h>


enum MLKReadtableCase
{
  MLKReadtableCase_UPCASE,
  MLKReadtableCase_DOWNCASE,
  MLKReadtableCase_INVERT,
  MLKReadtableCase_PRESERVE
};

enum MLKSyntaxType
{
  CONSTITUENT = 0,
  WHITESPACE = 1,
  TERMINATING_MACRO = 2,
  NONTERMINATING_MACRO = 3,
  SINGLE_ESCAPE = 4,
  MULTI_ESCAPE = 5
};

enum MLKConstituentTrait
{
  ALPHABETIC = 1,
  INVALID = 2,
  PACKAGE_MARKER = 4,
  ALPHA_DIGIT = 8,
  EXPONENT_MARKER = 16,
  NUMBER_MARKER = 32,
  RATIO_MARKER = 64,
  DECIMAL_POINT = 128,
  MINUS_SIGN = 256,
  PLUS_SIGN = 512,
  SIGN = 1024,
  DOT = 2048
};


@interface MLKReadtable : NSObject <NSCopying>
{
  NSMutableDictionary *_syntaxTable;
  NSMutableDictionary *_readerMacros;
  NSMutableDictionary *_traits;
  //id <MLKFuncallable> _caseConverter;
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
-(BOOL) isDecimalDigit:(unichar)ch;
-(BOOL) isDigit:(unichar)ch inBase:(int)base;
-(int) digitWeight:(unichar)ch;

-(id <MLKFuncallable>) macroFunctionForCharacter:(unichar)ch;
-(void) setMacroFunction:(id <MLKFuncallable>)function forCharacter:(unichar)ch;
-(unichar) charWithReadtableCase:(unichar)ch;

-(int) characterConstituentTraits:(unichar)ch;
-(BOOL) character:(unichar)ch
         hasTrait:(enum MLKConstituentTrait)trait;
-(enum MLKSyntaxType) characterSyntaxType:(unichar)ch;

-(void) setSyntaxType:(enum MLKSyntaxType)type
         forCharacter:(unichar)ch;
-(void) setConstituentTrait:(enum MLKConstituentTrait)trait
               forCharacter:(unichar)ch;
-(void) unsetConstituentTrait:(enum MLKConstituentTrait)trait
                 forCharacter:(unichar)ch;
@end
