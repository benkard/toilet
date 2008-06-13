/* -*- mode: objc; coding: utf-8 -*- */
/* Copyright 2008, Matthias Benkard. */

#import "MLKLispValue.h"


@interface MLKCons : MLKLispValue
{
  id _car;
  id _cdr;
}

+(MLKCons*) cons:(id)car with:(id)cdr;

-(MLKCons*) initWithCar:(id)car cdr:(id)cdr;

-(id) car;
-(id) cdr;
-(void) setCar:(id)value;
-(void) setCdr:(id)value;

-(void) dealloc;
@end
