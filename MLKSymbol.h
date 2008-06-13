/* -*- mode: objc; coding: utf-8 -*- */
/* Copyright 2008, Matthias Benkard. */

#import "MLKLispValue.h"

@class MLKPackage;


@interface MLKSymbol : MLKLispValue
{
  NSString *name;
  MLKPackage *homePackage;
}

-(MLKSymbol *) initWithName:(id)aName package:(id)aPackage;

-(NSString *) name;
-(MLKPackage *) homePackage;
-(void) setHomePackage:(MLKPackage *)aPackage;

-(void) dealloc;
@end
