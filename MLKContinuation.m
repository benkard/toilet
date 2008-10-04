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

#import "MLKContinuation.h"
#import "globals.h"
#import "util.h"


@implementation MLKContinuation
- (id)init
{
  self = [super init];
  _continuation = make_continuation (MLKRootContinuation);
  setjump (_continuation->jmpbuf);
  return self;
}

+ (id)continuation
{
  return LAUTORELEASE ([[self alloc] init]);
}

+ (NSArray *)callWithCurrentContinuation:(id <MLKFuncallable>)function
{
  id cont = [self continuation];
  return [function applyToArray:[NSArray arrayWithObject:cont]];
}

- (NSArray *)applyToArray:(NSArray *)arguments
{
  throw_to_continuation (_continuation, (long)arguments, MLKRootContinuation);
  return nil;
}

- (void)dealloc
{
  // ... _continuation->other ... (only if CONTINUATION_OTHER is defined in scmflags.h)
  free_continuation (_continuation);
  [super dealloc];
}
@end
