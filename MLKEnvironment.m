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

-(MLKEnvironment *) initWithParent:(MLKEnvironment *)parent
{
  ASSIGN (_bindings, parent->_bindings);
  return self;
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
@end
