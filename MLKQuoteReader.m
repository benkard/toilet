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

#import "MLKQuoteReader.h"

#import "MLKCons.h"
#import "MLKDynamicContext.h"
#import "MLKReader.h"
#import "MLKReadtable.h"
#import "MLKPackage.h"
#import "MLKStream.h"
#import "runtime-compatibility.h"
#import "util.h"


@implementation MLKQuoteReader
-(NSArray *) applyToArray:(NSArray *)arguments
{
  MLKStream *stream;
  MLKReadtable *readtable;
  MLKPackage *cl;
  unichar ch;
  id quoted_form;

  cl = [MLKPackage findPackage:@"COMMON-LISP"];

  stream = [arguments objectAtIndex:0];
  readtable = [[MLKDynamicContext currentContext]
                valueForSymbol:[cl intern:@"*READTABLE*"]];

  while ([readtable isWhitespaceCharacter:(ch = [stream readChar])]);

  [stream unreadChar:ch];

  quoted_form = [MLKReader readFromStream:stream
                           eofError:YES
                           eofValue:nil
                           recursive:YES
                           preserveWhitespace:NO];

  return [NSArray arrayWithObject:
                    [MLKCons cons:[cl intern:@"QUOTE"]
                             with:[MLKCons cons:nullify(quoted_form)
                                           with:nil]]];
}
@end
