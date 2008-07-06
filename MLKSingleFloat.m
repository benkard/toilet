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

#define _XOPEN_SOURCE 600  // strtof

#import "MLKSingleFloat.h"
#import "MLKDoubleFloat.h"
#import "runtime-compatibility.h"

#import <Foundation/NSString.h>

#include <stdio.h>
#include <stdlib.h>
#include <locale.h>


@implementation MLKSingleFloat
-(MLKSingleFloat *) initWithIntegerPart:(NSString *)intPart
                               negative:(BOOL)negative
                         fractionalPart:(NSString *)fractPart
                               exponent:(NSString *)exponent
                       exponentNegative:(BOOL)exponentNegative
{
  self = [super init];
  char *locale;

  // FIXME: This is probably not thread-safe.
  locale = setlocale (LC_NUMERIC, NULL);
  setlocale (LC_NUMERIC, "C");
  
  // strtof or sscanf -- is there a difference?
  sscanf ([[NSString stringWithFormat:@"%c%@.%@e%c%@",
                     (negative ? '-' : '+'),
                     intPart,
                     fractPart,
                     (exponentNegative ? '-' : '+'),
                     ([exponent length] > 0 ? (id)exponent : (id)@"0")]
            UTF8String],
          "%f",
          &value);

  setlocale (LC_NUMERIC, locale);

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

-(MLKNumber *) add:(MLKNumber *)arg
{
  if ([arg isKindOfClass:[MLKDoubleFloat class]])
    return [MLKDoubleFloat doubleFloatWithDouble:(value + [(MLKFloat*)arg doubleValue])];
  else
    return [MLKSingleFloat singleFloatWithFloat:(value + [(MLKFloat*)arg floatValue])];
}

-(MLKNumber *) subtract:(MLKNumber *)arg
{
  if ([arg isKindOfClass:[MLKDoubleFloat class]])
    return [MLKDoubleFloat doubleFloatWithDouble:(value - [(MLKFloat*)arg doubleValue])];
  else
    return [MLKSingleFloat singleFloatWithFloat:(value - [(MLKFloat*)arg floatValue])];
}

-(MLKNumber *) multiplyWith:(MLKNumber *)arg
{
  if ([arg isKindOfClass:[MLKDoubleFloat class]])
    return [MLKDoubleFloat doubleFloatWithDouble:(value * [(MLKFloat*)arg doubleValue])];
  else
    return [MLKSingleFloat singleFloatWithFloat:(value * [(MLKFloat*)arg floatValue])];
}

-(MLKNumber *) divideBy:(MLKNumber *)arg
{
  if ([arg isKindOfClass:[MLKDoubleFloat class]])
    return [MLKDoubleFloat doubleFloatWithDouble:(value / [(MLKFloat*)arg doubleValue])];
  else
    return [MLKSingleFloat singleFloatWithFloat:(value / [(MLKFloat*)arg floatValue])];
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

-(NSString *) descriptionForLisp
{
  return [self description];
}
@end
