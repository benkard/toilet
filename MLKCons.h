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

#import <Foundation/NSArray.h>


/*! @class MLKCons

@abstract A cons cell.

@discussion A cons cell (or simply: a cons) is an ordered pair whose first
element is called the car and whose second element is called the cdr of the cons.
Cons cells are mutable by default.

Note that nil is explicitely allowed as both the car and cdr of a cons cell.  In fact,
when representing linked lists using cons cells, nil in the cdr of the last cons cell
is what conventionally marks the end of a list.
*/
@interface MLKCons : NSObject <NSCopying>
{
  id _car;
  id _cdr;
}

/*! @method cons:with:

@abstract Cons two objects together.

@param car The car of the new cons cell.
@param cdr The cdr of the new cons cell.
@return A newly allocated cons.
*/
+(MLKCons*) cons:(id)car with:(id)cdr;


/*! @method listWithArray:
@abstract Make a linked list of cons cells out of an array. */
+(MLKCons*) listWithArray:(NSArray *)array;

/*! @method initWithCar:cdr:
@abstract Initialise a new cons cell with car and cdr.
*/
-(MLKCons*) initWithCar:(id)car cdr:(id)cdr;

/*! @method car
@abstract The car of the cons cell.
*/
-(id) car;

/*! @method cdr
@abstract The cdr of the cons cell.
*/
-(id) cdr;

/*! @method setCar:
@abstract Change the car of the cons cell.
*/
-(void) setCar:(id)value;

/*! @method setCdr:
@abstract Change the cdr of the cons cell.
*/
-(void) setCdr:(id)value;

/*! @method array
@abstract Return the content of the linked list represented by this cons cell as an array. */
-(NSArray *) array;

-(void) appendObject:(id)object;
-(MLKCons *) listByAppendingObject:(id)object;
-(MLKCons *) copyList;
-(int) length;

-(NSString *) bareDescriptionForLisp;  // description without
                                       // parentheses, for internal use
                                       // only
-(NSString *) descriptionForLisp;

-(id) copyWithZone:(NSZone *)zone;
-(BOOL) isEqual:(id)object;
  
-(void) dealloc;
@end
