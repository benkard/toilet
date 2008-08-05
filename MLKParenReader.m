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

#import "MLKParenReader.h"

#import "MLKCharacter.h"
#import "MLKCons.h"
#import "MLKDynamicContext.h"
#import "MLKReader.h"
#import "MLKReadtable.h"
#import "MLKPackage.h"
#import "MLKStream.h"
#import "runtime-compatibility.h"
#import "util.h"

#import <Foundation/NSArray.h>


static unichar slurpWhitespaceAndPeek (MLKStream *stream, MLKReadtable *readtable)
{
  unichar ch;
  while ([readtable isWhitespaceCharacter:(ch = [stream readChar])]);
  [stream unreadChar:ch];
  return ch;
}


@implementation MLKParenReader
-(NSArray *) applyToArray:(NSArray *)arguments
{
  MLKStream *stream;
  unichar ch;
  MLKReadtable *readtable;
  MLKCons *cons, *tail;
  unichar nextChar;
  
  stream = [arguments objectAtIndex:0];
  ch = [[arguments objectAtIndex:1] unicharValue];
  readtable = [[MLKDynamicContext currentContext]
                valueForSymbol:[[MLKPackage findPackage:@"COMMON-LISP"]
                                  intern:@"*READTABLE*"]];
  cons = nil;
  tail = nil;

  while ((nextChar = slurpWhitespaceAndPeek(stream, readtable)) != ')')
    {
      id item;
      id dotMarker = [[NSObject alloc] init];

      // FIXME: What to do about dots?  Maybe add a new
      // singleDotAllowed:(BOOL)dotp argument to readFromStream:...?
      item = [MLKReader readFromStream:stream
                        eofError:YES
                        eofValue:nil
                        recursive:YES
                        preserveWhitespace:NO
                        singleDotMarker:dotMarker
                        readingUninternedSymbol:NO];

      if (item == dotMarker)
        {
          id nextItem;

          LRELEASE (dotMarker);

          nextItem = [MLKReader readFromStream:stream
                                eofError:YES
                                eofValue:nil
                                recursive:YES
                                preserveWhitespace:NO];
          [tail setCdr:nextItem];

          if ((nextChar = slurpWhitespaceAndPeek (stream, readtable)) == ')')
            {
              [stream readChar];
              return [NSArray arrayWithObject:cons];
            }
          else
            {
              [NSException raise:@"MLKReaderError"
                           format:@"Unexpectedly read a single dot."];
            }
        }

      if (!tail)
        {
          cons = tail = [MLKCons cons:item with:nil];
        }
      else
        {
          [tail setCdr:[MLKCons cons:item with:nil]];
          tail = [tail cdr];
        }

      LRELEASE (dotMarker);
    }

  [stream readChar];
  
  return [NSArray arrayWithObject:(cons ? (id)cons : (id)[NSNull null])];
}
@end
