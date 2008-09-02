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

#include "MLKThrowException.h"
#import "runtime-compatibility.h"
#import "util.h"


@implementation MLKThrowException
-(id) initWithCatchTag:(MLKSymbol *)catchTag
                values:(NSArray *)values
{
  self = [super initWithName:@"MLKThrowException"
                reason:[NSString stringWithFormat:
                                   @"THROW: tag %@, values %@.",
                                   MLKPrintToString(catchTag),
                                   MLKPrintToString(values)]
                userInfo:nil];
  LASSIGN (_catchTag, catchTag);
  LASSIGN (_values, values);
  return self;
}

-(MLKSymbol *) catchTag
{
  return _catchTag;
}

-(NSArray *) thrownValues
{
  return _values;
}

-(void) dealloc
{
  LDESTROY (_catchTag);
  LDESTROY (_values);
  [super dealloc];
}
@end
