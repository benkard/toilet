/* -*- mode: objc; coding: utf-8 -*- */
/* Copyright 2008, Matthias Benkard. */

#import "MLKUndefinedVariableException.h"


@implementation MLKUndefinedVariableException
-(MLKUndefinedVariableException *) initWithEnvironment:(id)anEnvironment
                                          variableName:(id)aSymbol
{
  variableName = aSymbol;
  environment = anEnvironment;
  return self;
}
@end
