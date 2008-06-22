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

#import "MLKDynamicContext.h"
#import "MLKInteger.h"
#import "MLKPackage.h"
#import "runtime-compatibility.h"
#import "util.h"

#import <Foundation/NSString.h>

@implementation MLKInteger
-(MLKInteger *) initWithMPZ:(mpz_t)mpz
{
  self = [super init];
  mpz_init_set (value, mpz);
  return self;
}

-(MLKInteger *) initWithInt:(int)intValue
{
  self = [super init];
  mpz_init_set_si (value, intValue);
  return self;
}

-(MLKInteger *) initWithString:(NSString *)string
                      negative:(BOOL)negative
                          base:(unsigned int)base
{
  self = [super init];
  mpz_init_set_str (value, [string UTF8String], base);
  if (negative) mpz_neg (value, value);    
  return self;
}

+(MLKInteger *) integerWithMPZ:(mpz_t)mpz
{
  return AUTORELEASE ([[MLKInteger alloc] initWithMPZ:mpz]);
}

+(MLKInteger *) integerWithString:(NSString *)string
                         negative:(BOOL)negative
                             base:(unsigned int)base
{
  return AUTORELEASE ([[MLKInteger alloc] initWithString:string
                                          negative:negative
                                          base:base]);
}

+(MLKInteger *) integerWithInt:(int)intValue
{
  return AUTORELEASE ([[MLKInteger alloc] initWithInt:intValue]);
}


#define DEFINE_MPZ_TWOARG_OPERATION(SELECTOR, GMPFUN)                   \
  DEFINE_GMP_OPERATION (SELECTOR (MLKInteger *)arg,                     \
                        mpz,                                            \
                        GMPFUN (mpval, self->value, arg->value),        \
                        MLKInteger,                                     \
                        integerWithMPZ:)

DEFINE_MPZ_TWOARG_OPERATION (add:, mpz_add)
DEFINE_MPZ_TWOARG_OPERATION (subtract:, mpz_sub)
DEFINE_MPZ_TWOARG_OPERATION (multiplyWith:, mpz_mul)
DEFINE_MPZ_TWOARG_OPERATION (divideBy:, mpz_div)


-(int) intValue
{
  return mpz_get_si (value);
}

-(NSString *) description
{
  return [self descriptionWithBase:10];
}

-(NSString *) descriptionWithBase:(int)base
{
  NSString *str;
  char cstr[mpz_sizeinbase (self->value, base) + 2];

  mpz_get_str (cstr, base, self->value);
  str = [NSString stringWithUTF8String:cstr];
  
  return str;
}

-(NSString *) descriptionForLisp
{
  MLKInteger *base = [[MLKDynamicContext currentContext]
                       valueForBinding:[[MLKPackage
                                          findPackage:@"COMMON-LISP"]
                                         intern:@"*PRINT-BASE*"]];
  return [self descriptionWithBase:[base intValue]];
}

-(void) dealloc
{
  mpz_clear (value);
  [super dealloc];
}
@end
