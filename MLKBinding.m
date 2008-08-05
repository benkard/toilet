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

#import "MLKBinding.h"
#import "runtime-compatibility.h"
#import "util.h"

#import <Foundation/NSException.h>
#import <Foundation/NSString.h>


static id UNBOUND;


@implementation MLKBinding
+(void) initialize
{
  UNBOUND = [[NSObject alloc] init];
}

-(MLKBinding *) init
{
  return [self initWithValue:UNBOUND];
}

-(MLKBinding *) initWithValue:(id)something
{
  self = [super init];
  LASSIGN (value, something);
  return self;
}

+(MLKBinding *) binding
{
  return LAUTORELEASE ([[self alloc] init]);
}

+(MLKBinding *) bindingWithValue:(id)something
{
  return LAUTORELEASE ([[self alloc] initWithValue:something]);
}

-(void) setValue:(id)something
{
  LASSIGN (value, something);
}

-(id) value
{
  if (value == UNBOUND)
    [NSException raise:@"MLKUnboundVariableError"
                 format:@""];

  return value;
}

-(BOOL) boundp
{
  return !(value == UNBOUND);
}

-(void) makunbound
{
  LASSIGN (value, UNBOUND);
}

-(void) dealloc
{
  LRELEASE (value);
  [super dealloc];
}
@end
