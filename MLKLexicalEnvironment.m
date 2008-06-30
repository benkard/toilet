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
#import <Foundation/NSString.h>
#import <Foundation/NSThread.h>

#import "MLKCons.h"
#import "MLKEnvironment.h"
#import "MLKLexicalEnvironment.h"
#import "MLKLinkedList.h"
#import "MLKPackage.h"
#import "MLKParenReader.h"
#import "MLKReadtable.h"
#import "MLKSymbol.h"
#import "MLKInteger.h"
#import "runtime-compatibility.h"


#define MAKE_ENVIRONMENT(variable, parent, parent_member)               \
  (variable                                                             \
   ? (id) [[MLKEnvironment alloc]                                       \
            initWithParent:(parent                                      \
                            ? (id) parent_member                        \
                            : nil)                                      \
                    values:variable]                                    \
   : (id) (parent                                                       \
           ? (id) RETAIN (parent_member)                                \
           : [[MLKEnvironment alloc] init]));


static MLKLexicalEnvironment *global_environment;


@implementation MLKLexicalEnvironment
+(void) initialize
{
  NSMutableDictionary *vars = [NSMutableDictionary dictionary];
  NSMutableDictionary *funs = [NSMutableDictionary dictionary];

  MLKPackage *cl = [MLKPackage findPackage:@"COMMON-LISP"];
  //  MLKPackage *sys = [MLKPackage findPackage:@"TOILET-SYSTEM"];

  [vars setObject:[NSNull null] forKey:[NSNull null]];
  [vars setObject:[cl intern:@"T"] forKey:[cl intern:@"T"]];

  global_environment = [[self alloc] initWithParent:nil
                                     variables:vars
                                     functions:funs];
}

-(MLKLexicalEnvironment *) initWithParent:(MLKLexicalEnvironment *)aContext
                                variables:(NSDictionary *)vars
                                functions:(NSDictionary *)functions
{
  self = [super init];
  ASSIGN (_parent, (aContext ? aContext : global_environment));
  _variables = MAKE_ENVIRONMENT(vars, _parent, _parent->_variables);
  _functions = MAKE_ENVIRONMENT(functions, _parent, _parent->_functions);
  return self;
}

+(MLKLexicalEnvironment *) environmentWithParent:(MLKLexicalEnvironment *)context
                                       variables:(NSDictionary *)vars
                                       functions:(NSDictionary *)functions
{
  return AUTORELEASE ([[self alloc] initWithParent:context
                                    variables:vars
                                    functions:functions]);
}

+(MLKLexicalEnvironment *) globalEnvironment
{
  return global_environment;
}

-(NSSet *) variables
{
  return [_variables bindings];
}

-(NSSet *) functions
{
  return [_functions bindings];
}

-(id) valueForSymbol:(MLKSymbol *)symbol
{
  return [_variables valueForSymbol:symbol];
}

-(void) setValue:(id)value forSymbol:(MLKSymbol *)symbol
{
  [_variables setValue:value forSymbol:symbol];
}

-(void) addValue:(id)value forSymbol:(MLKSymbol *)symbol
{
  if (_parent && _variables == _parent->_variables)
    _variables = [[MLKEnvironment alloc] initWithParent:_parent->_variables
                                         values:nil];
  else if (!_variables)
    _variables = [[MLKEnvironment alloc] initWithParent:nil
                                         values:nil];

  [_variables addValue:value forSymbol:symbol];
}

-(void) addBindingForSymbol:(MLKSymbol *)symbol
{
  if (_parent && _variables == _parent->_variables)
    _variables = [[MLKEnvironment alloc] initWithParent:_parent->_variables
                                         values:nil];
  else if (!_variables)
    _variables = [[MLKEnvironment alloc] initWithParent:nil
                                         values:nil];

  [_variables addBindingForSymbol:symbol];
}

-(BOOL) boundp:(MLKSymbol *)symbol
{
  return [_variables boundp:symbol];
}

-(void) makunbound:(MLKSymbol *)symbol
{
  [_variables makunbound:symbol];
}

-(id) functionForSymbol:(MLKSymbol *)symbol
{
  return [_functions valueForSymbol:symbol];
}

-(void) setFunction:(id)value forSymbol:(MLKSymbol *)symbol
{
  [_functions setValue:value forSymbol:symbol];
}

-(void) addFunction:(id)value forSymbol:(MLKSymbol *)symbol
{
  if (_parent && _functions == _parent->_functions)
    _functions = [[MLKEnvironment alloc] initWithParent:_parent->_functions
                                         values:nil];
  else if (!_functions)
    _functions = [[MLKEnvironment alloc] initWithParent:nil
                                         values:nil];

  [_functions addValue:value forSymbol:symbol];
}

-(BOOL) fboundp:(MLKSymbol *)symbol
{
  return [_functions boundp:symbol];
}

-(void) fmakunbound:(MLKSymbol *)symbol
{
  [_functions makunbound:symbol];
}

-(void) dealloc
{
  RELEASE (_variables);
  RELEASE (_functions);
  RELEASE (_parent);
  [super dealloc];
}
@end
