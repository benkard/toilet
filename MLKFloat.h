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

#import "MLKLispValue.h"

#import <Foundation/NSString.h>

// An MLKFloat can represent any kind of CL floating-point number.
//
// MLKFloat itself is not instantiable.  It is a class cluster whose
// instances are of one of its subclasses (currently MLKSingleFloat und
// MLKDoubleFloat; an arbitrary-precision, GMP-backed type is planned
// for the future).
@interface MLKFloat : MLKLispValue
+(MLKFloat *) floatWithExponentMarker:(unichar)exponentMarker
                          integerPart:(NSString *)intPart
                             negative:(BOOL)negative
                       fractionalPart:(NSString *)fractPart
                             exponent:(NSString *)exponent
                     exponentNegative:(BOOL)exponentNegative;

// Abstract methods.
-(float) floatValue;
-(double) doubleValue;

-(MLKFloat *) add:(MLKFloat *)arg;
-(MLKFloat *) subtract:(MLKFloat *)arg;
-(MLKFloat *) multiplyWith:(MLKFloat *)arg;
-(MLKFloat *) divideBy:(MLKFloat *)arg;

-(NSString *) description;
@end
