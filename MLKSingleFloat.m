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

#import "MLKSingleFloat.h"
#import "MLKDoubleFloat.h"

#import <Foundation/NSString.h>

#include <stdio.h>


@implementation MLKSingleFloat
-(MLKSingleFloat *) initWithIntegerPart:(NSString *)intPart
                               negative:(BOOL)negative
                         fractionalPart:(NSString *)fractPart
                               exponent:(NSString *)exponent
                       exponentNegative:(BOOL)exponentNegative
{
  self = [super init];
  sscanf ([[NSString stringWithFormat:@"%c%@.%@e%c%@",
                     (negative ? '-' : '+'),
                     intPart,
                     fractPart,
                     (exponentNegative ? '-' : '+'),
                     ([exponent length] > 0 ? (id)exponent : (id)@"0")]
            UTF8String],
          "%f",
          &value);
  return self;
}

+(MLKSingleFloat *) singleFloatWithIntegerPart:(NSString *)intPart
                                      negative:(BOOL)negative
                                fractionalPart:(NSString *)fractPart
                                      exponent:(NSString *)exponent
                              exponentNegative:(BOOL)exponentNegative
{
  return AUTORELEASE ([[self alloc] initWithIntegerPart:intPart
                                    negative:negative
                                    fractionalPart:fractPart
                                    exponent:exponent
                                    exponentNegative:exponentNegative]);
}

-(MLKSingleFloat *) initWithFloat:(float)aFloat
{
  self = [super init];
  value = aFloat;
  return self;
}

+(MLKSingleFloat *) singleFloatWithFloat:(float)aFloat
{
  return AUTORELEASE ([[self alloc] initWithFloat:aFloat]);
}

-(float) floatValue
{
  return value;
}

-(double) doubleValue
{
  return value;
}

-(MLKFloat *) add:(MLKFloat *)arg
{
  if ([arg isKindOfClass:[MLKDoubleFloat class]])
    return [MLKDoubleFloat doubleFloatWithDouble:(value + [arg doubleValue])];
  else
    return [MLKSingleFloat singleFloatWithFloat:(value + [arg floatValue])];
}

-(MLKFloat *) subtract:(MLKFloat *)arg
{
  if ([arg isKindOfClass:[MLKDoubleFloat class]])
    return [MLKDoubleFloat doubleFloatWithDouble:(value - [arg doubleValue])];
  else
    return [MLKSingleFloat singleFloatWithFloat:(value - [arg floatValue])];
}

-(MLKFloat *) multiplyWith:(MLKFloat *)arg
{
  if ([arg isKindOfClass:[MLKDoubleFloat class]])
    return [MLKDoubleFloat doubleFloatWithDouble:(value * [arg doubleValue])];
  else
    return [MLKSingleFloat singleFloatWithFloat:(value * [arg floatValue])];
}

-(MLKFloat *) divideBy:(MLKFloat *)arg
{
  if ([arg isKindOfClass:[MLKDoubleFloat class]])
    return [MLKDoubleFloat doubleFloatWithDouble:(value / [arg doubleValue])];
  else
    return [MLKSingleFloat singleFloatWithFloat:(value / [arg floatValue])];
}

-(NSString *) description
{
  NSString *str = [NSString stringWithFormat:@"%e", value];
  int i;

  for (i = 0; i < [str length]; i++)
    {
      if ([str characterAtIndex:i] == 'e')
        {
          str = [NSString stringWithFormat:@"%@s%@",
                          [str substringToIndex:i],
                          [str substringFromIndex:(i+1)]];
          return str;
        }
    }

  return [NSString stringWithFormat:@"%@s0",str];
}
@end
