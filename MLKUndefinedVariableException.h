/* -*- mode: objc; coding: utf-8 -*- */
/* Copyright 2008, Matthias Benkard. */

#import <Foundation/NSException.h>

@class MLKSymbol, MLKEnvironment;


@interface MLKUndefinedVariableException : NSException
{
  MLKSymbol *variableName;
  MLKEnvironment *environment;
}

-(MLKUndefinedVariableException *) initWithEnvironment:(id)environment
                                          variableName:(id)symbol;
@end
