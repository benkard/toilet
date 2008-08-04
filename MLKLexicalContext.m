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

#import "MLKCons.h"
#import "MLKDynamicContext.h"
#import "MLKLexicalContext.h"
#import "MLKLexicalEnvironment.h"
#import "MLKEnvironment.h"
#import "MLKLinkedList.h"
#import "MLKPackage.h"
#import "MLKParenReader.h"
#import "MLKReadtable.h"
#import "MLKSymbol.h"
#import "MLKInteger.h"
#import "runtime-compatibility.h"
#import "util.h"


#define MAKE_ENVIRONMENT(variable, parent, parent_member)               \
  [[MLKEnvironment alloc]                                               \
    initWithParent:(parent                                              \
                    ? (id) parent_member                                \
                    : nil)                                              \
    values:variable]


static MLKLexicalContext *global_context;


static MLKPackage *cl;
static MLKPackage *sys;
static MLKSymbol *SPECIAL;
static MLKSymbol *LEXICAL;


@implementation MLKLexicalContext
+(void) initialize
{
  MLKLexicalEnvironment *globalenv = [MLKLexicalEnvironment globalEnvironment];
  cl = [MLKPackage findPackage:@"COMMON-LISP"];
  sys = [MLKPackage findPackage:@"TOILET-SYSTEM"];

  global_context = [[self alloc] initWithParent:nil
                                 variables:[globalenv variables]
                                 functions:[globalenv functions]
                                 goTags:nil
                                 macros:nil
                                 compilerMacros:nil
                                 symbolMacros:nil
                                 declarations:nil];

  SPECIAL = [cl intern:@"SPECIAL"];
  LEXICAL = [sys intern:@"LEXICAL"];
}

-(MLKLexicalContext *) initWithParent:(MLKLexicalContext *)aContext
                            variables:(NSSet *)vars
                            functions:(NSSet *)functions
                               goTags:(NSDictionary *)goTags
                               macros:(NSDictionary *)macros
                       compilerMacros:(NSDictionary *)compilerMacros
                         symbolMacros:(NSDictionary *)symbolMacros
                         declarations:(id)declarations
{
  self = [super init];

  LASSIGN (_parent, (aContext ? aContext : [MLKLexicalContext globalContext]));
  
  LASSIGN (_variables, [NSMutableSet setWithSet:vars]);
  LASSIGN (_functions, [NSMutableSet setWithSet:functions]);

  _goTags = MAKE_ENVIRONMENT (goTags, _parent, _parent->_goTags);
  _macros = MAKE_ENVIRONMENT (macros, _parent, _parent->_macros);
  _compilerMacros = MAKE_ENVIRONMENT (compilerMacros, _parent, _parent->_compilerMacros);
  _symbolMacros = MAKE_ENVIRONMENT (symbolMacros, _parent, _parent->_symbolMacros);

  LASSIGN (_knownMacros, [NSMutableSet setWithArray:[macros allKeys]]);
  LASSIGN (_knownSymbolMacros, [NSMutableSet setWithArray:[symbolMacros allKeys]]);

  LASSIGN (_declarations, declarations);
  return self;  
}

+(MLKLexicalContext *) contextWithParent:(MLKLexicalContext *)context
                               variables:(NSSet *)vars
                               functions:(NSSet *)functions
                                  goTags:(NSDictionary *)goTags
                                  macros:(NSDictionary *)macros
                          compilerMacros:(NSDictionary *)compilerMacros
                            symbolMacros:(NSDictionary *)symbolMacros
                            declarations:(id)declarations
{
  return LAUTORELEASE ([[self alloc]
                        initWithParent:context
                        variables:vars
                        functions:functions
                        goTags:goTags
                        macros:macros
                        compilerMacros:compilerMacros
                        symbolMacros:symbolMacros
                        declarations:declarations]);
}

+(MLKLexicalContext *) globalContext
{
  return global_context;
}

-(id <MLKFuncallable>) macroForSymbol:(MLKSymbol *)symbol
{
  return [_macros valueForSymbol:symbol];
}

-(void) addMacro:(id <MLKFuncallable>)value forSymbol:(MLKSymbol *)symbol
{
  [_knownMacros addObject:symbol];
  [_macros addValue:value forSymbol:symbol];
}

-(void) setMacro:(id <MLKFuncallable>)value forSymbol:(MLKSymbol *)symbol
{
  [_macros setValue:value forSymbol:symbol];
}

-(id <MLKFuncallable>) compilerMacroForSymbol:(MLKSymbol *)symbol
{
  return [_compilerMacros valueForSymbol:symbol];
}

-(void) addCompilerMacro:(id <MLKFuncallable>)value forSymbol:(MLKSymbol *)symbol
{
  [_knownCompilerMacros addObject:symbol];
  [_compilerMacros addValue:value forSymbol:symbol];
}

-(void) setCompilerMacro:(id <MLKFuncallable>)value forSymbol:(MLKSymbol *)symbol
{
  [_compilerMacros setValue:value forSymbol:symbol];
}

-(id <MLKFuncallable>) symbolMacroForSymbol:(MLKSymbol *)symbol
{
  return [_symbolMacros valueForSymbol:symbol];
}

-(void) addSymbolMacro:(id <MLKFuncallable>)value forSymbol:(MLKSymbol *)symbol
{
  [_knownSymbolMacros addObject:symbol];
  [_symbolMacros addValue:value forSymbol:symbol];
}

-(void) setSymbolMacro:(id <MLKFuncallable>)value forSymbol:(MLKSymbol *)symbol
{
  [_symbolMacros setValue:value forSymbol:symbol];
}

-(id) goTagForSymbol:(MLKSymbol *)symbol
{
  return [_goTags valueForSymbol:symbol];
}

-(id) declarations
{
  return _declarations;
}

-(void) addDeclaration:(id)declaration
{
  LASSIGN (_declarations,
          [MLKCons cons:declaration
                   with:_declarations]);
}

-(BOOL) symbolNamesFunction:(MLKSymbol *)symbol
{
  symbol = symbol ? (id)symbol : (id)[NSNull null];
  if ([_functions containsObject:symbol])
    return YES;
  else if ([_knownMacros containsObject:symbol])
    return NO;
  else
    return (_parent && [_parent symbolNamesFunction:symbol]);  
}

-(BOOL) symbolNamesMacro:(MLKSymbol *)symbol
{
  symbol = symbol ? (id)symbol : (id)[NSNull null];
  if ([_functions containsObject:symbol])
    return NO;
  else if ([_knownMacros containsObject:symbol])
    return YES;
  else
    return (_parent && [_parent symbolNamesMacro:symbol]);  
}

-(BOOL) symbolNamesSymbolMacro:(MLKSymbol *)symbol
{
  symbol = symbol ? (id)symbol : (id)[NSNull null];
  if ([_variables containsObject:symbol])
    return NO;
  else if ([_knownSymbolMacros containsObject:symbol])
    return YES;
  else
    return (_parent && [_parent symbolNamesSymbolMacro:symbol]);  
}

-(BOOL) variableIsLexical:(MLKSymbol *)symbol
{
  id rest;

  symbol = symbol ? (id)symbol : (id)[NSNull null];

  if ([_variables containsObject:symbol])
    {
      // The variable was introduced in this lexical context.
      rest = _declarations;
      while (rest)
        {
          id item = [rest car];
          if ([item isKindOfClass:[MLKCons class]] && [[item cdr] car] == symbol)
            {
              if ([item car] == LEXICAL)
                return YES;
              else if ([item car] == SPECIAL)
                return NO;
            }
          rest = [rest cdr];
        }

      // Has the variable been globally proclaimed special?
      rest = [MLKLexicalContext globalContext]->_declarations;
      while (rest)
        {
          id item = [rest car];
          if ([[item cdr] car] == symbol)
            {
              if ([item car] == LEXICAL)
                return YES;
              else if ([item car] == SPECIAL)
                return NO;
            }
          rest = [rest cdr];
        }

      // The variable is apparently neither locally nor pervasively
      // special.
      return YES;
    }
  // We don't know anything about a variable of the given name.  Ask the
  // parent environment.  If there is no parent, nobody seems to know
  // anything about the variable, so we assume it's a special one.
  else return (_parent && [_parent variableIsLexical:symbol]);
}

-(void) addVariable:(MLKSymbol *)symbol
{
  symbol = symbol ? (id)symbol : (id)[NSNull null];
  [_variables addObject:symbol];
}

-(void) addFunction:(MLKSymbol *)symbol
{
  symbol = symbol ? (id)symbol : (id)[NSNull null];
  [_functions addObject:symbol];
}

-(void) dealloc
{
  LRELEASE (_macros);
  LRELEASE (_compilerMacros);
  LRELEASE (_symbolMacros);
  LRELEASE (_knownMacros);
  LRELEASE (_knownCompilerMacros);
  LRELEASE (_knownSymbolMacros);
  LRELEASE (_goTags);
  LRELEASE (_functions);
  LRELEASE (_variables);
  LRELEASE (_declarations);
  LRELEASE (_parent);
  [super dealloc];
}
@end
