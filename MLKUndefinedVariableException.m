/* -*- mode: objc; coding: utf-8 -*- */
/* Copyright 2008, Matthias Benkard. */

#import "MLKUndefinedVariableException.h"


@implementation MLKUndefinedVariableException
-(MLKUndefinedVariableException *) initWithEnvironment:(id)anEnvironment
                                          variableName:(id)aSymbol
{
  self = [super init];
  variableName = aSymbol;
  environment = anEnvironment;
  return self;
}

-(void) dealloc
{
  RELEASE (variableName);
  RELEASE (environment);
  [super dealloc];
}
@end
