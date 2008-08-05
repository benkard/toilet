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

#define _XOPEN_SOURCE 600  // strtof (not actually needed here)

#import "MLKSingleFloat.h"
#import "MLKDoubleFloat.h"
#import "runtime-compatibility.h"
#import "util.h"

#import <Foundation/NSString.h>

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <locale.h>


@implementation MLKDoubleFloat
-(MLKDoubleFloat *) initWithIntegerPart:(NSString *)intPart
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

  // strtod or sscanf -- is there a difference?
  sscanf ([[NSString stringWithFormat:@"%c%@.%@e%c%@",
                     (negative ? '-' : '+'),
                     intPart,
                     fractPart,
                     (exponentNegative ? '-' : '+'),
                     ([exponent length] > 0 ? (id)exponent : (id)@"0")]
            UTF8String],
          "%lf",
          &value);
  
  setlocale (LC_NUMERIC, locale);

  return self;
}

+(MLKDoubleFloat *) doubleFloatWithIntegerPart:(NSString *)intPart
                                      negative:(BOOL)negative
                                fractionalPart:(NSString *)fractPart
                                      exponent:(NSString *)exponent
                              exponentNegative:(BOOL)exponentNegative
{
  return LAUTORELEASE ([[self alloc] initWithIntegerPart:intPart
                                    negative:negative
                                    fractionalPart:fractPart
                                    exponent:exponent
                                    exponentNegative:exponentNegative]);
}

-(MLKDoubleFloat *) initWithDouble:(double)aDouble
{
  self = [super init];
  value = aDouble;
  return self;
}

+(MLKDoubleFloat *) doubleFloatWithDouble:(double)aDouble
{
  return LAUTORELEASE ([[self alloc] initWithDouble:aDouble]);
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
  return [MLKDoubleFloat doubleFloatWithDouble:(value + [(MLKFloat*)arg doubleValue])];
}

-(MLKNumber *) subtract:(MLKNumber *)arg
{
  return [MLKDoubleFloat doubleFloatWithDouble:(value - [(MLKFloat*)arg doubleValue])];
}

-(MLKNumber *) multiplyWith:(MLKNumber *)arg
{
  return [MLKDoubleFloat doubleFloatWithDouble:(value * [(MLKFloat*)arg doubleValue])];
}

-(MLKNumber *) divideBy:(MLKNumber *)arg
{
  return [MLKDoubleFloat doubleFloatWithDouble:(value / [(MLKFloat*)arg doubleValue])];
}

-(NSComparisonResult) compare:(MLKDoubleFloat *)arg
{
  if (self->value == arg->value)
    return NSOrderedSame;
  else if (self->value < arg->value)
    return NSOrderedAscending;
  else
    return NSOrderedDescending;
}

-(BOOL) isEqual:(id)arg
{
  return ([arg isKindOfClass:[MLKDoubleFloat class]]
          && self->value == ((MLKDoubleFloat *)arg)->value);
}


#define DEFINE_NULLARY_OPERATOR(NAME)                                   \
  -(MLKDoubleFloat *) NAME                                              \
  {                                                                     \
    return [MLKDoubleFloat doubleFloatWithDouble:(NAME (value))];       \
  }

DEFINE_NULLARY_OPERATOR (sin);
DEFINE_NULLARY_OPERATOR (cos);
DEFINE_NULLARY_OPERATOR (tan);
DEFINE_NULLARY_OPERATOR (asin);
DEFINE_NULLARY_OPERATOR (acos);
DEFINE_NULLARY_OPERATOR (atan);
DEFINE_NULLARY_OPERATOR (sinh);
DEFINE_NULLARY_OPERATOR (cosh);
DEFINE_NULLARY_OPERATOR (tanh);
DEFINE_NULLARY_OPERATOR (exp);
DEFINE_NULLARY_OPERATOR (log);
DEFINE_NULLARY_OPERATOR (sqrt);
DEFINE_NULLARY_OPERATOR (ceil);
DEFINE_NULLARY_OPERATOR (floor);


-(MLKDoubleFloat *) pow:(MLKDoubleFloat *)exponent
{
  return [MLKDoubleFloat doubleFloatWithDouble:
                           (pow (self->value, exponent->value))];
}

-(NSString *) description
{
  NSString *str = [NSString stringWithFormat:@"%e", value];
  int i;

  for (i = 0; i < [str length]; i++)
    {
      if ([str characterAtIndex:i] == 'e')
        {
          str = [NSString stringWithFormat:@"%@d%@",
                          [str substringToIndex:i],
                          [str substringFromIndex:(i+1)]];
          return str;
        }
    }

  return [NSString stringWithFormat:@"%@d0",str];
}

-(NSString *) descriptionForLisp
{
  return [self description];
}
@end
