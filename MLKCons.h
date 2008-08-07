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

#import "MLKLispValue.h"

@class NSArray;


@interface MLKCons : MLKLispValue <NSCopying>
{
  id _car;
  id _cdr;
}

+(MLKCons*) cons:(id)car with:(id)cdr;
+(MLKCons*) listWithArray:(NSArray *)array;

-(MLKCons*) initWithCar:(id)car cdr:(id)cdr;

-(id) car;
-(id) cdr;
-(void) setCar:(id)value;
-(void) setCdr:(id)value;

-(NSArray *) array;

-(void) appendObject:(id)object;
-(MLKCons *) listByAppendingObject:(id)object;
-(MLKCons *) copyList;

-(NSString *) bareDescriptionForLisp;  // description without
                                       // parentheses, for internal use
                                       // only
-(NSString *) descriptionForLisp;

-(id) copyWithZone:(NSZone *)zone;
-(BOOL) isEqual:(id)object;
  
-(void) dealloc;
@end
