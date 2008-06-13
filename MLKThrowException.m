/* -*- mode: objc; coding: utf-8 -*- */
/* Copyright 2008, Matthias Benkard. */

#include "MLKThrowException.h"


@implementation MLKThrowException
-(MLKThrowException *) initWithCatchTag:(MLKSymbol *)catchTag
                                  value:(id)value
{
  _catchTag = catchTag;
  _value = value;
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
@end
