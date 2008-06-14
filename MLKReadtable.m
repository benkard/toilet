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

#import <Foundation/NSDictionary.h>
#import <Foundation/NSValue.h>


#define CONSTITUENT 0
#define WHITESPACE 1
#define TERMINATING_MACRO 2
#define NONTERMINATING_MACRO 3
#define SINGLE_ESCAPE 4
#define MULTI_ESCAPE 5


@implementation MLKReadtable
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
  MLKReadtable *copy = [MLKReadtable allocWithZone:zone];
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

-(BOOL) isNonTerminatingMacroCharacter:(unichar)ch;
{
  return ([[_syntaxTable objectForKey:[NSNumber numberWithLong:ch]]
            isEqual:[NSNumber numberWithShort:NONTERMINATING_MACRO]]);
}

-(BOOL) isTerminatingMacroCharacter:(unichar)ch;
{
  return ([[_syntaxTable objectForKey:[NSNumber numberWithLong:ch]]
            isEqual:[NSNumber numberWithShort:TERMINATING_MACRO]]);
}

-(BOOL) isSingleEscapeCharacter:(unichar)ch;
{
  return ([[_syntaxTable objectForKey:[NSNumber numberWithLong:ch]]
            isEqual:[NSNumber numberWithShort:SINGLE_ESCAPE]]);
}

-(BOOL) isMultipleEscapeCharacter:(unichar)ch;
{
  return ([[_syntaxTable objectForKey:[NSNumber numberWithLong:ch]]
            isEqual:[NSNumber numberWithShort:MULTI_ESCAPE]]);
}

-(BOOL) isConstituentCharacter:(unichar)ch;
{
  return ([[_syntaxTable objectForKey:[NSNumber numberWithLong:ch]]
            isEqual:[NSNumber numberWithShort:CONSTITUENT]]);
}

-(BOOL) isInvalidCharacter:(unichar)ch;
{
  return ([_syntaxTable objectForKey:[NSNumber numberWithLong:ch]] == nil);
}

-(BOOL) characterHasCase:(unichar)ch
{
  return (![[[NSString stringWithFormat:@"%C", ch] uppercaseString]
             isEqual:[[NSString stringWithFormat:@"%C", ch] lowercaseString]]);
}

-(MLKClosure *) macroFunctionForCharacter:(unichar)ch;
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
      return [[[NSString stringWithFormat:@"%C", ch] uppercaseString]
               characterAtIndex:0];
    case MLKReadtableCase_DOWNCASE:
      return [[[NSString stringWithFormat:@"%C", ch] lowercaseString]
               characterAtIndex:0];
    case MLKReadtableCase_INVERT:
      {
        unichar upCh;
        upCh = [[[NSString stringWithFormat:@"%C", ch] uppercaseString]
                characterAtIndex:0];
        if (ch == upCh)
          return [[[NSString stringWithFormat:@"%C", ch] lowercaseString]
                   characterAtIndex:0];
        else
          return upCh;
      }
    }
  return 0;
}
@end
