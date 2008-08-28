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

#import "MLKDispatchingMacroCharacterReader.h"

#import "MLKDynamicContext.h"
#import "MLKInteger.h"
#import "MLKReader.h"
#import "MLKReadtable.h"
#import "MLKPackage.h"
#import "MLKCharacterStream.h"
#import "runtime-compatibility.h"
#import "util.h"

#import <Foundation/NSValue.h>


@implementation MLKDispatchingMacroCharacterReader
-(id) init
{
  self = [super init];
  LASSIGN (_readerMacros, [NSMutableDictionary dictionary]);
  return self;
}

-(id <MLKFuncallable>) macroFunctionForCharacter:(unichar)ch
{
  return [_readerMacros objectForKey:[NSNumber numberWithLong:ch]];
}

-(void) setMacroFunction:(id <MLKFuncallable>)function forCharacter:(unichar)ch
{
  [_readerMacros setObject:function
                 forKey:[NSNumber numberWithLong:ch]];
}

-(NSArray *) applyToArray:(NSArray *)arguments
{
  MLKCharacterStream *stream;
  MLKReadtable *readtable;
  MLKPackage *cl;
  unichar ch;
  id <MLKFuncallable> function;
  NSMutableString *prefix;

  cl = [MLKPackage findPackage:@"COMMON-LISP"];

  stream = [arguments objectAtIndex:0];
  readtable = [[MLKDynamicContext currentContext]
                valueForSymbol:[cl intern:@"*READTABLE*"]];

  prefix = [NSMutableString string];
  while ([readtable isDecimalDigit:(ch = [stream readChar])])
    [prefix appendFormat:@"%C", ch];

  function = [self macroFunctionForCharacter:ch];

  if (!function)
    [NSException raise:@"MLKSyntaxError"
                 format:@"There is no such dispatch macro subcharacter as %C.", ch];

  return [function applyToArray:
                     [NSArray arrayWithObjects:
                                nullify(stream),
                                [MLKCharacter characterWithUnichar:ch],
                                ([prefix length] > 0
                                 ? (id)[MLKInteger integerWithString:prefix
                                                   negative:NO
                                                   base:10]
                                 : (id)[NSNull null]),
                                nil]];
}
@end
