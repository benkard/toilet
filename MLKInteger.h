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

#import "MLKNumber.h"

#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <gmp.h>

@class NSString;


@interface MLKInteger : MLKNumber
{
  mpz_t value;
}

-(MLKInteger *) initWithMPZ:(mpz_t)mpz;
-(MLKInteger *) initWithString:(NSString *)string
                      negative:(BOOL)negative
                          base:(unsigned int)base;
-(MLKInteger *) initWithInt:(int)intValue;
-(MLKInteger *) initWithIntptr_t:(intptr_t)intptr_t_value;
-(MLKInteger *) initWithFixnum:(id)fixnum;

+(MLKInteger *) integerWithMPZ:(mpz_t)mpz;
+(MLKInteger *) integerWithString:(NSString *)string
                         negative:(BOOL)negative
                             base:(unsigned int)base;
+(MLKInteger *) integerWithInt:(int)intValue;
+(MLKInteger *) integerWithIntptr_t:(intptr_t)intptr_t_value;
+(MLKInteger *) integerWithFixnum:(id)fixnum;

-(BOOL) fitsIntoFixnum;
-(id) fixnumValue;

-(int) intValue;
-(double) doubleValue;

-(MLKNumber *) add:(MLKNumber *)arg;
-(MLKNumber *) subtract:(MLKNumber *)arg;
-(MLKNumber *) multiplyWith:(MLKNumber *)arg;
-(MLKNumber *) divideBy:(MLKNumber *)arg;

-(NSComparisonResult) compare:(MLKInteger *)arg;
-(BOOL) isEqual:(id)arg;

-(BOOL) evenp;
-(BOOL) oddp;

-(MLKInteger *) mod:(MLKInteger *)arg;
-(MLKInteger *) exactlyDivideBy:(MLKInteger *)arg;
-(MLKInteger *) isqrt;
-(MLKInteger *) pow:(MLKInteger *)exponent;
-(MLKInteger *) gcd:(MLKInteger *)arg;
-(MLKInteger *) lcm:(MLKInteger *)arg;

-(NSString *) description;
-(NSString *) descriptionWithBase:(int)base;
-(NSString *) descriptionForLisp;

-(void) dealloc;
@end
