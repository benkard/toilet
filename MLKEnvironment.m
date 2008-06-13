/* -*- mode: objc; coding: utf-8 -*- */
/* Copyright 2008, Matthias Benkard. */

#import <Foundation/NSDictionary.h>
#import <Foundation/NSArray.h>

#import "MLKEnvironment.h"
#import "MLKUndefinedVariableException.h"


@implementation MLKEnvironment
-(MLKEnvironment *) init
{
  return [self initWithParent:nil bindings:nil];
}

-(MLKEnvironment *) initWithParent:(MLKEnvironment *)parent bindings:(NSDictionary *)bindings
{
  _bindings = [[NSMutableDictionary alloc] initWithCapacity:10];
  ASSIGN (_parent, parent);
  [self addBindings: bindings];
  return self;
}

-(MLKEnvironment *) initWithParent:(MLKEnvironment *)parent
{
  return [self initWithParent:parent bindings:nil];
}

-(MLKEnvironment *) initWithBindings:(NSDictionary *)bindings
{
  return [self initWithParent:nil bindings:bindings];
}

-(void) setBinding:(MLKSymbol *)symbol to:(id)value
{
  [self setBinding:symbol to:value inEnvironment:self];
}

-(void) setBinding:(MLKSymbol *)symbol to:(id)value inEnvironment:(MLKEnvironment *)env
{
  if ([[_bindings allKeys] containsObject:symbol])
    [_bindings setObject:value forKey:symbol];
  else
    if (_parent)
      [_parent setBinding:symbol to:value inEnvironment:env];
    else
      [[[MLKUndefinedVariableException alloc] initWithEnvironment:env
                                              variableName:symbol]
        raise];
}

-(id) valueForBinding:(MLKSymbol *)symbol
{
  return [self valueForBinding:symbol];
}

-(id) valueForBinding:(MLKSymbol *)symbol inEnvironment:(MLKEnvironment *)env
{
  if ([[_bindings allKeys] containsObject:symbol])
    return [_bindings objectForKey:symbol];
  else
    if (_parent)
      return [_parent valueForBinding:symbol];
    else
      [[[MLKUndefinedVariableException alloc] initWithEnvironment:env
                                              variableName:symbol]
        raise];;
  
  return nil;  // avoid a stupid compiler warning
}

-(void) addBindings:(NSDictionary *)bindings
{
  [_bindings addEntriesFromDictionary:bindings];
}

-(void) addBinding:(MLKSymbol *)symbol to:(id)value
{
  [_bindings setObject:value forKey:symbol];
}

-(void) dealloc
{
  RELEASE (_bindings);
  RELEASE (_parent);
  [super dealloc];
}
@end
