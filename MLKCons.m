/* -*- mode: objc; coding: utf-8 -*- */
/* Copyright 2008, Matthias Benkard. */

#import "MLKCons.h"


@implementation MLKCons
+(MLKCons*) cons:(id)car with:(id)cdr
{
  return AUTORELEASE ([[MLKCons alloc] initWithCar:car cdr:cdr]);
}

-(MLKCons*) initWithCar:(id)car cdr:(id)cdr
{
  ASSIGN (_car, car);
  ASSIGN (_cdr, cdr);
  return self;
}


-(id) car
{
  return _car;
}

-(id) cdr
{
  return _cdr;
}

-(void) setCar:(id)value
{
  ASSIGN (_car, value);
}

-(void) setCdr:(id)value
{
  ASSIGN (_cdr, value);
}

-(void) dealloc
{
  RELEASE (_car);
  RELEASE (_cdr);
  [super dealloc];
}
@end
