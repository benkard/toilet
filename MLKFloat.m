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
#import "MLKSingleFloat.h"
#import "MLKDoubleFloat.h"
#import "MLKPackage.h"
#import "MLKDynamicContext.h"
#import "runtime-compatibility.h"

#import <Foundation/NSException.h>


@implementation MLKFloat
+(MLKFloat *) floatWithExponentMarker:(unichar)exponentMarker
                          integerPart:(NSString *)intPart
                             negative:(BOOL)negative
                       fractionalPart:(NSString *)fractPart
                             exponent:(NSString *)exponent
                     exponentNegative:(BOOL)exponentNegative
{
  MLKSymbol *defaultFormat;
  MLKPackage *cl;

  cl = [MLKPackage findPackage:@"COMMON-LISP"];
  defaultFormat = [[MLKDynamicContext currentContext]
                    valueForSymbol:[cl intern:@"*READ-DEFAULT-FLOAT-FORMAT*"]];

  // FIXME: Shouldn't the readtable decide which exponent markers do what?
  if (exponentMarker == 'd' || exponentMarker == 'D'
      || exponentMarker == 'l' || exponentMarker == 'L'
      || ((exponentMarker == 'e' || exponentMarker == 'E')
          && ((defaultFormat == [cl intern:@"DOUBLE-FLOAT"])
              || (defaultFormat == [cl intern:@"LONG-FLOAT"]))))
    return [MLKDoubleFloat doubleFloatWithIntegerPart:intPart
                           negative:negative
                           fractionalPart:fractPart
                           exponent:exponent
                           exponentNegative:exponentNegative];
  else
    return [MLKSingleFloat singleFloatWithIntegerPart:intPart
                           negative:negative
                           fractionalPart:fractPart
                           exponent:exponent
                           exponentNegative:exponentNegative];
}

#define DECLARE_ABSTRACT(SIGNATURE, RETURN_VALUE)                       \
  SIGNATURE                                                             \
  {                                                                     \
    [NSException raise:@"MLKInternalInconsistencyError"                 \
                 format:@"Tried to invoke an abstract method."];        \
    return RETURN_VALUE;                                                \
  }

DECLARE_ABSTRACT (-(float) floatValue, 0.0)
DECLARE_ABSTRACT (-(double) doubleValue, 0.0)
DECLARE_ABSTRACT (-(MLKNumber *) add:(MLKNumber *)arg, nil)
DECLARE_ABSTRACT (-(MLKNumber *) subtract:(MLKNumber *)arg, nil)
DECLARE_ABSTRACT (-(MLKNumber *) multiplyWith:(MLKNumber *)arg, nil)
DECLARE_ABSTRACT (-(MLKNumber *) divideBy:(MLKNumber *)arg, nil)

-(NSString *) description
{
  return [super description];
}
@end
