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

#import "MLKFloat.h"

@class NSString;


@interface MLKDoubleFloat : MLKFloat
{
  double value;
}

-(MLKDoubleFloat *) initWithIntegerPart:(NSString *)intPart
                               negative:(BOOL)negative
                         fractionalPart:(NSString *)fractPart
                               exponent:(NSString *)exponent
                       exponentNegative:(BOOL)exponentNegative;

+(MLKDoubleFloat *) doubleFloatWithIntegerPart:(NSString *)intPart
                                      negative:(BOOL)negative
                                fractionalPart:(NSString *)fractPart
                                      exponent:(NSString *)exponent
                              exponentNegative:(BOOL)exponentNegative;

-(MLKDoubleFloat *) initWithDouble:(double)aDouble;
+(MLKDoubleFloat *) doubleFloatWithDouble:(double)aDouble;

-(float) floatValue;
-(double) doubleValue;

-(MLKNumber *) add:(MLKNumber *)arg;
-(MLKNumber *) subtract:(MLKNumber *)arg;
-(MLKNumber *) multiplyWith:(MLKNumber *)arg;
-(MLKNumber *) divideBy:(MLKNumber *)arg;

-(NSComparisonResult) compare:(MLKDoubleFloat *)arg;
-(BOOL) isEqual:(id)arg;

-(MLKDoubleFloat *) sin;
-(MLKDoubleFloat *) cos;
-(MLKDoubleFloat *) tan;
-(MLKDoubleFloat *) asin;
-(MLKDoubleFloat *) acos;
-(MLKDoubleFloat *) atan;
-(MLKDoubleFloat *) sinh;
-(MLKDoubleFloat *) cosh;
-(MLKDoubleFloat *) tanh;
-(MLKDoubleFloat *) exp;
-(MLKDoubleFloat *) log;
-(MLKDoubleFloat *) sqrt;
-(MLKDoubleFloat *) ceil;
-(MLKDoubleFloat *) floor;
-(MLKDoubleFloat *) pow:(MLKDoubleFloat *)exponent;

-(NSString *) description;
-(NSString *) descriptionForLisp;
@end
