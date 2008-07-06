/* -*- mode: objc; coding: utf-8 -*- */
/* Étoilisp/Mulklisp, a Common Lisp subset for the Étoilé runtime.
 * Copyright (C) 2008  Matthias Andreas Benkard.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at
 * your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#import <Foundation/NSArray.h>
#import <Foundation/NSDictionary.h>
#import <Foundation/NSException.h>
#import <Foundation/NSNull.h>
#import <Foundation/NSSet.h>

#import "MLKEnvironment.h"
#import "NSObject-MLKPrinting.h"
#import "runtime-compatibility.h"


@implementation MLKEnvironment
-(MLKEnvironment *) init
{
  return [self initWithParent:nil values:nil];
}

-(MLKEnvironment *) initWithParent:(MLKEnvironment *)parent
                            values:(NSDictionary *)bindings
{
  self = [super init];
  _bindings = [[NSMutableDictionary alloc] initWithCapacity:10];
  ASSIGN (_parent, parent);
  [self addValues:bindings];
  return self;
}

-(MLKEnvironment *) initWithParent:(MLKEnvironment *)parent
{
  return [self initWithParent:parent values:nil];
}

-(MLKEnvironment *) initWithValues:(NSDictionary *)bindings
{
  return [self initWithParent:nil values:bindings];
}

-(MLKEnvironment *) parent
{
  return _parent;
}

-(NSArray *) bindingArray
{
  NSMutableArray *array = [NSMutableArray arrayWithArray:[_bindings allKeys]];

  if (_parent)
    [array addObjectsFromArray:[_parent bindingArray]];

  return array;
}

-(NSSet *) bindings
{
  return [NSSet setWithArray:[self bindingArray]];
}

-(void) setValue:(id)value forSymbol:(MLKSymbol *)symbol;
{
  MLKBinding *binding;

  if (!(binding = [self bindingForSymbol:symbol]))
    [NSException raise:@"MLKUnboundVariableError"
                 format:@"The variable %@ is unbound.",
                       [symbol descriptionForLisp]];

  [binding setValue:value];
}

-(id) valueForSymbol:(MLKSymbol *)symbol
{
  MLKBinding *binding;

  if (!(binding = [self bindingForSymbol:symbol]))
    [NSException raise:@"MLKUnboundVariableError"
                 format:@"The variable %@ is unbound.",
                       [symbol descriptionForLisp]];
      
  return [binding value];
}

-(MLKBinding*) bindingForSymbol:(MLKSymbol *)symbol
{
  MLKBinding *binding;
  
  symbol = symbol ? (id)symbol : (id)[NSNull null];

  if ((binding = [_bindings objectForKey:symbol]))
    return binding;
  else
    if (_parent)
      return [_parent bindingForSymbol:symbol];
    else
      return nil;
}

-(void) addBindings:(NSDictionary *)bindings
{
  [_bindings addEntriesFromDictionary:bindings];
}

-(void) addValues:(NSDictionary *)bindings
{
  int i;
  NSArray *keys;

  keys = [bindings allKeys];
  for (i = 0; i < [keys count]; i++)
    {
      id key = [keys objectAtIndex:i];
      id value = [bindings objectForKey:key];

      value = (value == [NSNull null]) ? nil : value;

      [_bindings setObject:[MLKBinding bindingWithValue:value]
                 forKey:key];
    }
}

-(void) addValue:(id)value forSymbol:(MLKSymbol *)symbol;
{
  symbol = symbol ? (id)symbol : (id)[NSNull null];
  [_bindings setObject:[MLKBinding bindingWithValue:value]
             forKey:symbol];
}

-(void) addBindingForSymbol:(MLKSymbol *)symbol
{
  [self addBinding:[MLKBinding binding] forSymbol:symbol];
}

-(void) addBinding:(MLKBinding *)binding forSymbol:(MLKSymbol *)symbol
{
  [_bindings setObject:binding
             forKey:(symbol ? (id)symbol : (id)[NSNull null])];
}

-(void) setBinding:(MLKBinding *)binding forSymbol:(MLKSymbol *)symbol
{
  if (![self bindingForSymbol:symbol])
    [NSException raise:@"MLKUnboundVariableError"
                 format:@"The variable %@ is unbound.",
                        [symbol descriptionForLisp]];
  [self addBinding:binding forSymbol:symbol];
}

-(MLKEnvironment *) environmentForSymbol:(MLKSymbol *)symbol
{
  if ([_bindings objectForKey:symbol])
    return self;
  else if (_parent)
    return [_parent environmentForSymbol:symbol];
  else
    return nil;
}

-(BOOL) boundp:(MLKSymbol *)symbol
{
  MLKBinding *binding;

  if ((binding = [self bindingForSymbol:symbol]))
    return [binding boundp];
  else
    return NO;
}

-(void) makunbound:(MLKSymbol *)symbol
{
  MLKBinding *binding;

  if ((binding = [self bindingForSymbol:symbol]))
    [binding makunbound];
}

-(void) dealloc
{
  RELEASE (_bindings);
  RELEASE (_parent);
  [super dealloc];
}
@end
