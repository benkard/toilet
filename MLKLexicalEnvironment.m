/* -*- mode: objc; coding: utf-8 -*- */
/* Toilet Lisp, a Common Lisp subset for the Étoilé runtime.
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

#import "MLKCompiledClosure.h"
#import "MLKCons.h"
#import "MLKEnvironment.h"
#import "MLKLexicalContext.h"
#import "MLKLexicalEnvironment.h"
#import "MLKPackage.h"
#import "MLKParenReader.h"
#import "MLKReadtable.h"
#import "MLKSymbol.h"
#import "MLKInteger.h"
#import "MLKValuesFunction.h"
#import "runtime-compatibility.h"
#import "util.h"


#define MAKE_ENVIRONMENT(variable, parent, parent_member)               \
  [[MLKEnvironment alloc]                                               \
    initWithParent:(parent                                              \
                    ? (id) parent_member                                \
                    : nil)                                              \
    values:variable]


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

  [funs setObject:LAUTORELEASE ([[MLKValuesFunction alloc] init])
        forKey:[cl intern:@"VALUES"]];

  global_environment = [[self alloc] initWithParent:nil
                                     variables:vars
                                     functions:funs];
}

-(MLKLexicalEnvironment *) initWithParent:(MLKLexicalEnvironment *)aContext
                                variables:(NSDictionary *)vars
                                functions:(NSDictionary *)functions
{
  self = [super init];
  LASSIGN (_parent, (aContext ? aContext : global_environment));
  _variables = MAKE_ENVIRONMENT(vars, _parent, _parent->_variables);
  _functions = MAKE_ENVIRONMENT(functions, _parent, _parent->_functions);
  return self;
}

+(MLKLexicalEnvironment *) environmentWithParent:(MLKLexicalEnvironment *)context
                                       variables:(NSDictionary *)vars
                                       functions:(NSDictionary *)functions
{
  return LAUTORELEASE ([[self alloc] initWithParent:context
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
  if (![_variables environmentForSymbol:symbol]
      || [_variables environmentForSymbol:symbol] == global_environment->_variables)
    {
      id cell = [[MLKLexicalContext globalContext] bindingForSymbol:symbol];
      return [cell value];
    }
  else
    {
      return [_variables valueForSymbol:symbol];
    }
}

-(void) setValue:(id)value forSymbol:(MLKSymbol *)symbol
{
  if (![_variables environmentForSymbol:symbol]
      || [_variables environmentForSymbol:symbol] == global_environment->_variables)
    {
      id cell = [[MLKLexicalContext globalContext] bindingForSymbol:symbol];
      [cell setValue:value];
    }
  else
    {
      [_variables setValue:value forSymbol:symbol];
    }
}

-(void) addValue:(id)value forSymbol:(MLKSymbol *)symbol
{
  if (self == global_environment)
    {
      id cell = [[MLKLexicalContext globalContext] bindingForSymbol:symbol];
      [cell setValue:value];
    }
  else
    {
      [_variables addValue:value forSymbol:symbol];
    }
}

-(void) addBindingForSymbol:(MLKSymbol *)symbol
{
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

  if ([_functions environmentForSymbol:symbol] == global_environment->_functions)
    {
      // If we're changing the global environment, we need to
      // interoperate with compiled code.  In this case, be sure to set
      // the global function cell.
      //
      // Note that this reserves memory for the function cell that is
      // never freed, which is why we do it for global function bindings
      // only!
      id (**cell)(void *, ...) = [[MLKLexicalContext globalContext]
                                   functionCellForSymbol:symbol];
      void **closure_data_cell = [[MLKLexicalContext globalContext]
                                   closureDataPointerForSymbol:symbol];
      if ([value isKindOfClass:[MLKCompiledClosure class]])
        {
          *cell = (id (*)(void *, ...))[value code];
          *closure_data_cell = [value closureData];
        }
      else
        {
          *cell = MLKInterpretedFunctionTrampoline;
          *closure_data_cell = value;
        }
    }
}

-(void) addFunction:(id)value forSymbol:(MLKSymbol *)symbol
{
  [_functions addValue:value forSymbol:symbol];

  if (self == global_environment)
    {
      // If we're changing the global environment, we need to
      // interoperate with compiled code.  In this case, be sure to set
      // the global function cell.
      //
      // Note that this reserves memory for the function cell that is
      // never freed, which is why we do it for global function bindings
      // only!
      id (**cell)(void *, ...) = [[MLKLexicalContext globalContext]
                                   functionCellForSymbol:symbol];
      void **closure_data_cell = [[MLKLexicalContext globalContext]
                                   closureDataPointerForSymbol:symbol];
      if ([value isKindOfClass:[MLKCompiledClosure class]])
        {
          *cell = (id (*)(void *, ...))[value code];
          *closure_data_cell = [value closureData];
        }
      else
        {
          *cell = MLKInterpretedFunctionTrampoline;
          *closure_data_cell = value;
        }
    }
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
  LRELEASE (_variables);
  LRELEASE (_functions);
  LRELEASE (_parent);
  [super dealloc];
}
@end
