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


@interface MLKSingleFloat : MLKFloat
{
  float value;
}

-(MLKSingleFloat *) initWithIntegerPart:(NSString *)intPart
                               negative:(BOOL)negative
                         fractionalPart:(NSString *)fractPart
                               exponent:(NSString *)exponent
                       exponentNegative:(BOOL)exponentNegative;

+(MLKSingleFloat *) singleFloatWithIntegerPart:(NSString *)intPart
                                      negative:(BOOL)negative
                                fractionalPart:(NSString *)fractPart
                                      exponent:(NSString *)exponent
                              exponentNegative:(BOOL)exponentNegative;

-(float) floatValue;
-(double) doubleValue;

-(MLKFloat *) add:(MLKFloat *)arg;
-(MLKFloat *) subtract:(MLKFloat *)arg;
-(MLKFloat *) multiplyWith:(MLKFloat *)arg;
-(MLKFloat *) divideBy:(MLKFloat *)arg;

-(NSString *) description;
-(NSString *) descriptionForLisp;
@end
