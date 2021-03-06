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

#import "MLKDynamicContext.h"
#import "MLKPackage.h"
#import "MLKRatio.h"
#import "runtime-compatibility.h"
#import "util.h"

#import <Foundation/NSString.h>

@implementation MLKRatio
-(MLKRatio *) initWithMPQ:(mpq_t)mpq
{
  self = [super init];
  mpq_init (value);
  mpq_set (value, mpq);
  return self;
}

+(MLKRatio *) ratioWithMPQ:(mpq_t)mpq
{
  return LAUTORELEASE ([[MLKRatio alloc] initWithMPQ:mpq]);
}

-(MLKRatio *) initWithString:(NSString *)string
                        base:(unsigned int)base
{
  self = [super init];
  mpq_init (value);
  mpq_set_str (value, [string UTF8String], 10);
  mpq_canonicalize (value);
  return self;
}

-(MLKRatio *) initWithNumeratorString:(NSString *)numerString
                    denominatorString:(NSString *)denomString
                             negative:(BOOL)negative
                                 base:(unsigned int)base
{
  return [self initWithString:[NSString stringWithFormat:@"%s%@/%@",
                                        (negative ? "-" : ""),
                                        numerString,
                                        denomString]
               base:base];
}
  
+(MLKRatio *) ratioWithNumeratorString:(NSString *)numerString
                     denominatorString:(NSString *)denomString
                              negative:(BOOL)negative
                                  base:(unsigned int)base
{
  return LAUTORELEASE ([[MLKRatio alloc] initWithNumeratorString:numerString
                                        denominatorString:denomString
                                        negative:negative
                                        base:base]);
}

#define DEFINE_MPQ_TWOARG_OPERATION(SELECTOR, GMPFUN)                         \
  DEFINE_GMP_OPERATION (SELECTOR (MLKNumber *)arg,                            \
                        mpq,                                                  \
                        GMPFUN (mpval, self->value, ((MLKRatio*)arg)->value), \
                        MLKNumber,                                            \
                        MLKRatio,                                             \
                        ratioWithMPQ:)

DEFINE_MPQ_TWOARG_OPERATION (add:, mpq_add)
DEFINE_MPQ_TWOARG_OPERATION (subtract:, mpq_sub)
DEFINE_MPQ_TWOARG_OPERATION (multiplyWith:, mpq_mul)
DEFINE_MPQ_TWOARG_OPERATION (divideBy:, mpq_div)

-(double) doubleValue
{
  return mpq_get_d (value);
}

-(MLKNumber *) numerator
{
  mpz_t numer;
  MLKInteger *obj;

  mpz_init_set (numer, mpq_numref (self->value));
  obj = [MLKInteger integerWithMPZ:numer];
  mpz_clear (numer);

  return obj;
}

-(MLKNumber *) denominator
{
  mpz_t denom;
  MLKInteger *obj;

  mpz_init_set (denom, mpq_denref (self->value));
  obj = [MLKInteger integerWithMPZ:denom];
  mpz_clear (denom);

  return obj;
}

-(NSComparisonResult) compare:(MLKRatio *)arg
{
  int cmp = mpq_cmp (self->value, arg->value);

  if (cmp == 0)
    return NSOrderedSame;
  else if (cmp < 0)
    return NSOrderedAscending;
  else
    return NSOrderedDescending;
}

-(BOOL) isEqual:(id)arg
{
  return ([arg isKindOfClass:[MLKRatio class]]
          && mpq_equal (self->value, ((MLKRatio *)arg)->value));
}

-(NSString *) description
{
  return [self descriptionWithBase:10];
}

-(NSString *) descriptionWithBase:(int)base
{
  NSString *str;
  char cstr[mpz_sizeinbase (mpq_numref(self->value), base)
            + mpz_sizeinbase (mpq_denref(self->value), base)
            + 3];

  mpq_get_str (cstr, base, self->value);
  str = [NSString stringWithUTF8String:cstr];
  
  return str;
}

-(NSString *) descriptionForLisp
{
  MLKInteger *base = [[MLKDynamicContext currentContext]
                       valueForSymbol:[[MLKPackage
                                          findPackage:@"COMMON-LISP"]
                                         intern:@"*PRINT-BASE*"]];
  return [self descriptionWithBase:[base intValue]];
}

-(void) dealloc
{
  mpq_clear (value);
  [super dealloc];
}
@end
