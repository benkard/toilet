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

#include <stdarg.h>
#include <stdio.h>
#include <gmp.h>

@class NSString;


@interface MLKInteger : MLKLispValue
{
  mpz_t value;
}

-(MLKInteger *) initWithMPZ:(mpz_t)mpz;
-(MLKInteger *) initWithString:(NSString *)string
                      negative:(BOOL)negative
                          base:(unsigned int)base;

+(MLKInteger *) integerWithMPZ:(mpz_t)mpz;
+(MLKInteger *) integerWithString:(NSString *)string
                         negative:(BOOL)negative
                             base:(unsigned int)base;

-(int) intValue;

-(MLKInteger *) add:(MLKInteger *)arg;
-(MLKInteger *) subtract:(MLKInteger *)arg;
-(MLKInteger *) multiplyWith:(MLKInteger *)arg;
-(MLKInteger *) divideBy:(MLKInteger *)arg;

-(NSString *) description;
-(NSString *) descriptionWithBase:(int)base;

-(void) dealloc;
@end
