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
-(MLKThrowException *) initWithCatchTag:(MLKSymbol *)catchTag
                                  value:(id)value
{
  self = [super init];
  LASSIGN (_catchTag, catchTag);
  LASSIGN (_value, value);
  return self;
}

-(MLKSymbol *) catchTag
{
  return _catchTag;
}

-(id) value
{
  return _value;
}

-(void) dealloc
{
  LRELEASE (_catchTag);
  LRELEASE (_value);
  [super dealloc];
}
@end
