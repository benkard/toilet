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

#import "MLKLinkedList.h"
#import "MLKCons.h"
#import "runtime-compatibility.h"
#import "util.h"


@implementation MLKLinkedList
-(MLKLinkedList*) init
{
  self = [super init];
  _firstCons = nil;
  return self;
}

-(MLKLinkedList*) initWithCons:(MLKCons*)cons
{
  self = [super init];
  LASSIGN (_firstCons, cons);
  return self;
}

-(void) push: (id)object
{
  LASSIGN (_firstCons, [MLKCons cons:object with:_firstCons]);
}

-(id) pop
{
  id retval = [_firstCons car];
  LRETAIN (retval);
  LASSIGN (_firstCons, [_firstCons cdr]);
  LAUTORELEASE (retval);
  return retval;
}

-(MLKCons*) firstCons
{
  return _firstCons;
}

-(BOOL) null
{
  return !_firstCons;
}

#ifdef __OBJC2__
-(NSUInteger) countByEnumeratingWithState:(NSFastEnumerationState *)state objects:(id *)stackbuf count:(NSUInteger)len
{
  if (state->state == 0)
    {
      state->mutationsPtr = &_firstCons;
      state->extra[0] = (unsigned long) &_firstCons;
    }
  
  MLKCons *currentCons = (MLKCons*) state->extra[0];
  if (currentCons)
    {
      state->itemsPtr = [currentCons car];
      return 1;
    }
  else
    return 0;
}
#endif

-(void) dealloc
{
  LRELEASE (_firstCons);
  [super dealloc];
}
@end
