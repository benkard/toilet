/* -*- mode: objc; coding: utf-8 -*- */
/* Copyright 2008, Matthias Benkard. */

#import "MLKSymbol.h"


@implementation MLKSymbol
-(MLKSymbol *) initWithName:(id)aName package:(id)aPackage
{
  self = [super init];
  ASSIGN (name, aName);
  ASSIGN (homePackage, aPackage);
  return self;
}

-(NSString *) name
{
  return name;
}

-(MLKPackage *) homePackage
{
  return homePackage;
}

-(void) setHomePackage:(MLKPackage *)aPackage
{
  ASSIGN (homePackage, aPackage);
}

-(void) dealloc
{
  RELEASE (name);
  RELEASE (homePackage);
  [super dealloc];
}
@end
