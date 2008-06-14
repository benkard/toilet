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
