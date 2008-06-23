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
#import <Foundation/NSNull.h>
#import <Foundation/NSSet.h>

#import "MLKEnvironment.h"
#import "MLKUndefinedVariableException.h"
#import "runtime-compatibility.h"


static id UNBOUND;


@implementation MLKEnvironment
+(void) initialize
{
  UNBOUND = [[NSObject alloc] init];
}

-(MLKEnvironment *) init
{
  return [self initWithParent:nil bindings:nil];
}

-(MLKEnvironment *) initWithParent:(MLKEnvironment *)parent bindings:(NSDictionary *)bindings
{
  self = [super init];
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

-(MLKEnvironment *) parent
{
  return _parent;
}

-(NSSet *) bindings
{
  NSSet *set = [NSSet setWithArray:[_bindings allKeys]];
  return (_parent
          ? (id)[set setByAddingObjectsFromSet:[_parent bindings]]
          : (id)set);
}

-(void) setValue:(id)value forBinding:(MLKSymbol *)symbol;
{
  [self setBinding:symbol to:value inEnvironment:self];
}

-(void) setBinding:(MLKSymbol *)symbol to:(id)value inEnvironment:(MLKEnvironment *)env
{
  value = value ? value : (id) [NSNull null];
  if ([_bindings objectForKey:symbol])
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
  return [self valueForBinding:symbol inEnvironment:self];
}

-(id) valueForBinding:(MLKSymbol *)symbol inEnvironment:(MLKEnvironment *)env
{
  id value;
  if ((value = [_bindings objectForKey:symbol]))
    {
      if (value == [NSNull null])
        return nil;
      else if (value == UNBOUND)
        [[[MLKUndefinedVariableException alloc] initWithEnvironment:env
                                                variableName:symbol]
          raise];
      else
        return value;
    }
  else
    if (_parent)
      return [_parent valueForBinding:symbol];
    else
      [[[MLKUndefinedVariableException alloc] initWithEnvironment:env
                                              variableName:symbol]
        raise];

  return nil;  // avoid a stupid compiler warning
}

-(void) addBindings:(NSDictionary *)bindings
{
  [_bindings addEntriesFromDictionary:bindings];
}

-(void) addValue:(id)value forBinding:(MLKSymbol *)symbol;
{
  value = value ? value : (id) [NSNull null];
  [_bindings setObject:value forKey:symbol];
}

-(void) addBinding:(MLKSymbol *)symbol
{
  [_bindings setObject:UNBOUND forKey:symbol];
}

-(MLKEnvironment *) environmentForBinding:(MLKSymbol *)symbol
{
  if ([_bindings objectForKey:symbol])
    return self;
  else if (_parent)
    return [_parent environmentForBinding:symbol];
  else
    return nil;
}

-(BOOL) boundp:(MLKSymbol *)symbol
{
  id value;
  if ((value = [_bindings objectForKey:symbol]))
    return (value != UNBOUND);
  else if (_parent)
    return [_parent boundp:symbol];
  else
    return NO;
}

-(void) makunbound:(MLKSymbol *)symbol
{
  if ([_bindings objectForKey:symbol])
    [_bindings setObject:UNBOUND forKey:symbol];
  else if (_parent)
    return [_parent makunbound:symbol];
}

-(void) dealloc
{
  RELEASE (_bindings);
  RELEASE (_parent);
  [super dealloc];
}
@end
