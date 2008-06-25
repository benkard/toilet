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


#define MAKE_ENVIRONMENT(variable, parent, parent_member)               \
  (variable                                                             \
   ? (id) [[MLKEnvironment alloc]                                       \
            initWithParent:(parent                                      \
                            ? (id) parent_member                        \
                            : nil)                                      \
                    values:variable]                                    \
   : (id) (parent ? (id) RETAIN (parent_member) : nil));


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
                         symbolMacros:(NSDictionary *)symbolMacros
                         declarations:(id)declarations
{
  self = [super init];

  ASSIGN (_parent, (aContext ? aContext : [MLKLexicalContext globalContext]));
  
  ASSIGN (_variables, [NSMutableSet setWithSet:vars]);
  ASSIGN (_functions, [NSMutableSet setWithSet:functions]);

  _goTags = MAKE_ENVIRONMENT (goTags, _parent, _parent->_goTags);
  _macros = MAKE_ENVIRONMENT (macros, _parent, _parent->_macros);
  _symbolMacros = MAKE_ENVIRONMENT (macros, _parent, _parent->_symbolMacros);

  ASSIGN (_knownMacros, [macros allKeys]);
  ASSIGN (_knownSymbolMacros, [symbolMacros allKeys]);

  ASSIGN (_declarations, declarations);
  return self;  
}

+(MLKLexicalContext *) globalContext
{
  return global_context;
}

-(id) macroForSymbol:(MLKSymbol *)symbol
{
  return [_macros valueForSymbol:symbol];
}

-(void) setMacro:(MLKFuncallable *)value forSymbol:(MLKSymbol *)symbol
{
  [_symbolMacros setValue:value forSymbol:symbol];
}

-(id) symbolMacroForSymbol:(MLKSymbol *)symbol
{
  return [_symbolMacros valueForSymbol:symbol];
}

-(void) setSymbolMacro:(MLKFuncallable *)value forSymbol:(MLKSymbol *)symbol
{
  [_symbolMacros setValue:value forSymbol:symbol];
}

-(id) goTagForSymbol:(MLKSymbol *)symbol
{
  return [_goTags valueForSymbol:symbol];
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

  return YES;
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
  RELEASE (_macros);
  RELEASE (_knownMacros);
  RELEASE (_knownSymbolMacros);
  RELEASE (_symbolMacros);
  RELEASE (_goTags);
  RELEASE (_functions);
  RELEASE (_variables);
  RELEASE (_declarations);
  RELEASE (_parent);
  [super dealloc];
}
@end
