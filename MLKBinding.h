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

#import <Foundation/NSObject.h>


@interface MLKBinding : NSObject
{
  id value;
}

+(void) initialize;

/* Function: -init

  Initalise unbound binding cell.
*/
-(MLKBinding *) init;

/* Function: -initWithValue:

  Initialise binding cell with a value to bind.
*/
-(MLKBinding *) initWithValue:(id)something;

/* Function: +binding

  Construct an unbound binding cell.
*/
+(MLKBinding *) binding;

/* Function: +bindingWithValue:

  Construct a bound binding cell with a value to bind.
*/
+(MLKBinding *) bindingWithValue:(id)something;

/* Function: -setValue:

  Change the binding's bound value.
*/
-(void) setValue:(id)something;

/* Function: -value

  Access the binding's bound value.
*/
-(id) value;

/* Function: -boundp

  Determine whether the binding cell is non-empty.
*/
-(BOOL) boundp;

/* Function: -makunbound

  Empty the binding.
*/
-(void) makunbound;

-(void) dealloc;
@end
