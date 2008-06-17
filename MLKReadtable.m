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

#import "MLKReadtable.h"
#import "MLKCharacter.h"

#import <Foundation/NSDictionary.h>
#import <Foundation/NSValue.h>


#define CONSTITUENT 0
#define WHITESPACE 1
#define TERMINATING_MACRO 2
#define NONTERMINATING_MACRO 3
#define SINGLE_ESCAPE 4
#define MULTI_ESCAPE 5


enum MLKCharacterTrait
{
  ALPHABETIC = 0,
  INVALID,
  PACKAGE_MARKER,
  ALPHA_DIGIT,
  EXPONENT_MARKER,
  NUMBER_MARKER,
  RATIO_MARKER,
  DECIMAL_POINT,
  MINUS_SIGN,
  PLUS_SIGN,
  SIGN,
  DOT
};


@implementation MLKReadtable
-(MLKReadtable *) initSuper
{
  return [super init];
}

-(MLKReadtable *) init
{
  self = [super init];
  _syntaxTable = [[NSMutableDictionary alloc] init];
  _readerMacros = [[NSMutableDictionary alloc] init];
  _case = MLKReadtableCase_UPCASE;
  return self;
}

-(MLKReadtable *) copyWithZone:(NSZone *)zone
{
  MLKReadtable *copy = [[MLKReadtable allocWithZone:zone] initSuper];
  copy->_syntaxTable = [_syntaxTable mutableCopyWithZone:zone];
  copy->_readerMacros = [_readerMacros mutableCopyWithZone:zone];
  copy->_case = _case;
  return copy;
}

-(BOOL) isWhitespaceCharacter:(unichar)ch
{
  return ([[_syntaxTable objectForKey:[NSNumber numberWithLong:ch]]
            isEqual:[NSNumber numberWithShort:WHITESPACE]]);
}

-(BOOL) isMacroCharacter:(unichar)ch;
{
  return ([self isNonTerminatingMacroCharacter:ch] ||
          [self isTerminatingMacroCharacter:ch]);
}


#define DEFINE_SYNTAX_PREDICATE(SELECTOR, SYNTAX_TYPE)                  \
  -(BOOL) SELECTOR (unichar)ch                                          \
  {                                                                     \
    return ([[_syntaxTable objectForKey:[NSNumber numberWithLong:ch]]   \
              isEqual:[NSNumber numberWithShort:SYNTAX_TYPE]]);         \
  }

DEFINE_SYNTAX_PREDICATE(isNonTerminatingMacroCharacter:, NONTERMINATING_MACRO)
DEFINE_SYNTAX_PREDICATE(isTerminatingMacroCharacter:, TERMINATING_MACRO)
DEFINE_SYNTAX_PREDICATE(isSingleEscapeCharacter:, SINGLE_ESCAPE)
DEFINE_SYNTAX_PREDICATE(isMultipleEscapeCharacter:, MULTI_ESCAPE)
DEFINE_SYNTAX_PREDICATE(isConstituentCharacter:, CONSTITUENT)


-(BOOL) characterHasCase:(unichar)ch
{
  return (![[[NSString stringWithFormat:@"%C", ch] uppercaseString]
             isEqual:[[NSString stringWithFormat:@"%C", ch] lowercaseString]]);
}

-(MLKFuncallable *) macroFunctionForCharacter:(unichar)ch;
{
  return [_readerMacros objectForKey:[NSNumber numberWithLong:ch]];
}

-(unichar) charWithReadtableCase:(unichar)ch
{
  switch (_case)
    {
    case MLKReadtableCase_PRESERVE:
      return ch;
    case MLKReadtableCase_UPCASE:
      return [MLKCharacter uppercaseCharForChar:ch];
    case MLKReadtableCase_DOWNCASE:
      return [MLKCharacter lowercaseCharForChar:ch];
    case MLKReadtableCase_INVERT:
      {
        unichar upCh;
        upCh = [MLKCharacter uppercaseCharForChar:ch];
        if (ch == upCh)
          return [MLKCharacter lowercaseCharForChar:ch];
        else
          return upCh;
      }
    }
  return 0;
}


#define DEFINE_TRAIT_PREDICATE(SELECTOR, TRAIT)                   \
  -(BOOL) SELECTOR (unichar)ch                                    \
  {                                                               \
    return ([[_traits objectForKey:[NSNumber numberWithLong:ch]]  \
              isEqual:[NSNumber numberWithShort:TRAIT]]);         \
  }

DEFINE_TRAIT_PREDICATE(isInvalid:, INVALID)
DEFINE_TRAIT_PREDICATE(isAlphabetic:, ALPHABETIC)
DEFINE_TRAIT_PREDICATE(isPackageMarker:, PACKAGE_MARKER)
DEFINE_TRAIT_PREDICATE(isAlphaDigit:, ALPHA_DIGIT)
DEFINE_TRAIT_PREDICATE(isExponentMarker:, EXPONENT_MARKER)
DEFINE_TRAIT_PREDICATE(isNumberMarker:, NUMBER_MARKER)
DEFINE_TRAIT_PREDICATE(isRatioMarker:, RATIO_MARKER)
DEFINE_TRAIT_PREDICATE(isDecimalPoint:, DECIMAL_POINT)
DEFINE_TRAIT_PREDICATE(isMinusSign:, MINUS_SIGN)
DEFINE_TRAIT_PREDICATE(isPlusSign:, PLUS_SIGN)
DEFINE_TRAIT_PREDICATE(isSign:, SIGN)
DEFINE_TRAIT_PREDICATE(isDot:, DOT)


-(BOOL) isDecimalDigit:(unichar)ch
{
  return [self isDigit:ch inBase:10];
}

-(BOOL) isDigit:(unichar)ch inBase:(int)base
{
  if (base < 11)
    return (ch < '0' + base);
  else
    return (ch <= '9'
            || ('A' <= ch && ch < 'A' + base - 10)
            || ('a' <= ch && ch < 'a' + base - 10));
}

-(int) digitWeight:(unichar)ch
{
  if ('0' <= ch && ch <= '9')
    return (ch - '0');
  else if ('A' <= ch && ch <= 'Z')
    return (ch - 'A' + 10);
  else
    return (ch - 'a' + 10);
}
@end
