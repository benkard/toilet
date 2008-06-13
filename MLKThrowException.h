/* -*- mode: objc; coding: utf-8 -*- */
/* Copyright 2008, Matthias Benkard. */

#include <Foundation/NSException.h>

@class MLKSymbol;


@interface MLKThrowException : NSException
{
  MLKSymbol *_catchTag;
  id _value;
}

-(MLKThrowException *) initWithCatchTag:(MLKSymbol *)catchTag
                                  value:(id)value;

-(MLKSymbol *) catchTag;
-(id) value;

-(void) dealloc;
@end
