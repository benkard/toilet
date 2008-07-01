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

#import "MLKStringReader.h"

#import "MLKCharacter.h"
#import "MLKCons.h"
#import "MLKDynamicContext.h"
#import "MLKReader.h"
#import "MLKReadtable.h"
#import "MLKPackage.h"
#import "MLKStream.h"
#import "runtime-compatibility.h"

#import <Foundation/NSString.h>


@implementation MLKStringReader
-(NSArray *) applyToArray:(NSArray *)arguments
{
  MLKStream *stream;
  unichar ch;
  MLKReadtable *readtable;
  unichar nextChar;
  NSMutableString *string;

  stream = [arguments objectAtIndex:0];
  ch = [[arguments objectAtIndex:1] unicharValue];
  readtable = [[MLKDynamicContext currentContext]
                valueForSymbol:[[MLKPackage findPackage:@"COMMON-LISP"]
                                  intern:@"*READTABLE*"]];
  string = [NSMutableString string];

  while ((nextChar = [stream readChar]) != ch)
    {
      if ([readtable isSingleEscapeCharacter:nextChar])
        [string appendFormat:@"%C", [stream readChar]];
      else
        [string appendFormat:@"%C", nextChar];
    }

  return [NSArray arrayWithObject:string];
}
@end
