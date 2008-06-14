/* -*- mode: objc; coding: utf-8 -*- */
/* Copyright 2008, Matthias Benkard. */

#include "MLKThrowException.h"


@implementation MLKThrowException
-(MLKThrowException *) initWithCatchTag:(MLKSymbol *)catchTag
                                  value:(id)value
{
  self = [super init];
  ASSIGN (_catchTag, catchTag);
  ASSIGN (_value, value);
  return self;
}

-(MLKSymbol *) catchTag
{
  return _catchTag;
}

-(id) value
{
  return _value;
}

-(void) dealloc
{
  RELEASE (_catchTag);
  RELEASE (_value);
  [super dealloc];
}
@end
