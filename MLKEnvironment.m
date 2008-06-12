/* -*- mode: objc; coding: utf-8 -*- */
/* Copyright 2008, Matthias Benkard. */

#import <Foundation/NSDictionary.h>
#import <Foundation/NSArray.h>

#import "MLKEnvironment.h"
#import "MLKLinkedList.h"
#import "MLKCons.h"
#import "MLKUndefinedVariableException.h"


@implementation MLKEnvironment
-(MLKEnvironment *) init
{
  _bindings = [[MLKLinkedList alloc] init];
  return self;
}

-(MLKEnvironment *) initWithParent:(MLKEnvironment *)parent bindings:(NSDictionary *)bindings
{
  _bindings = [[MLKLinkedList alloc] initWithCons:[parent->_bindings firstCons]];
  [_bindings push: [NSMutableDictionary dictionaryWithCapacity:10]];
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
  MLKCons *cons;
  for (cons = [_bindings firstCons]; cons; cons = [cons cdr])
    {
      NSMutableDictionary *dict = [cons car];
      if ([[dict allKeys] containsObject:symbol])
        {
          [dict setObject:value forKey:symbol];
          break;
        }
    }
  
  [[[MLKUndefinedVariableException alloc] initWithEnvironment: self
                                          variableName: symbol]
    raise];
}

-(id) valueForBinding:(MLKSymbol *)symbol
{
  MLKCons *cons;
  for (cons = [_bindings firstCons]; cons; cons = [cons cdr])
    {
      NSMutableDictionary *dict = [cons car];
      if ([[dict allKeys] containsObject:symbol])
        {
          return [dict objectForKey:symbol];
        }
    }

  [[[MLKUndefinedVariableException alloc] initWithEnvironment: self
                                          variableName: symbol]
    raise];
  return nil;
}

-(void) addBindings:(NSDictionary *)bindings
{
  int i;
  NSArray *keys = [bindings allKeys];
  int count = [keys count];
  for (i = 0; i < count; i++)
    {
      [self addBinding:[keys objectAtIndex:i] to:[bindings objectForKey:[keys objectAtIndex:i]]];
    }
}

-(void) addBinding:(MLKSymbol *)symbol to:(id)value
{
  NSMutableDictionary *dict = [[_bindings firstCons] car];
  [dict setObject:value forKey:symbol];
}
@end
