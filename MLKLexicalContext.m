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
                  bindings:variable]                                    \
   : (id) (parent ? (id) RETAIN (parent_member) : nil));


static MLKLexicalContext *global_context;


static MLKPackage *cl;
static MLKPackage *sys;
static MLKSymbol *SPECIAL;
static MLKSymbol *LEXICAL;


@implementation MLKLexicalContext
+(void) initialize
{
  MLKDynamicContext *dynamic_context = [MLKDynamicContext globalContext];
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
  int i;
  NSArray *e;

  self = [super init];

  ASSIGN (_parent, (aContext ? aContext : [MLKLexicalContext globalContext]));
  
  ASSIGN (_variableLocations, [NSMutableDictionary dictionary]);
  e = [vars allObjects];
  for (i = 0; i < [e count]; i++)
    {
      [self addVariable:[e objectAtIndex:i]];
    }
  
  ASSIGN (_functionLocations, [NSMutableDictionary dictionary]);
  e = [functions allObjects];
  for (i = 0; i < [e count]; i++)
    {
      [self addFunction:[e objectAtIndex:i]];
    }

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
  return [_macros valueForBinding:symbol];
}

-(void) setMacro:(MLKFuncallable *)value forSymbol:(MLKSymbol *)symbol
{
  [_symbolMacros setValue:value forBinding:symbol];
}

-(id) symbolMacroForSymbol:(MLKSymbol *)symbol
{
  return [_symbolMacros valueForBinding:symbol];
}

-(void) setSymbolMacro:(MLKFuncallable *)value forSymbol:(MLKSymbol *)symbol
{
  [_symbolMacros setValue:value forBinding:symbol];
}

-(id) goTagForSymbol:(MLKSymbol *)symbol
{
  return [_goTags valueForBinding:symbol];
}

-(BOOL) symbolNamesFunction:(MLKSymbol *)symbol
{
  if ([_functionLocations objectForKey:(symbol ? (id)symbol : (id)[NSNull null])])
    return YES;
  else if ([_knownMacros containsObject:(symbol ? (id)symbol : (id)[NSNull null])])
    return NO;
  else
    return (_parent && [_parent symbolNamesFunction:symbol]);  
}

-(BOOL) symbolNamesMacro:(MLKSymbol *)symbol
{
  if ([_functionLocations objectForKey:(symbol ? (id)symbol : (id)[NSNull null])])
    return NO;
  else if ([_knownMacros containsObject:(symbol ? (id)symbol : (id)[NSNull null])])
    return YES;
  else
    return (_parent && [_parent symbolNamesMacro:symbol]);  
}

-(BOOL) symbolNamesSymbolMacro:(MLKSymbol *)symbol
{
  if ([_variableLocations objectForKey:(symbol ? (id)symbol : (id)[NSNull null])])
    return NO;
  else if ([_knownSymbolMacros containsObject:(symbol ? (id)symbol : (id)[NSNull null])])
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
      if ([[item cdr] car] == symbol)
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
  [_variableLocations setObject:[NSNull null] forKey:symbol];
}

-(void) addFunction:(MLKSymbol *)symbol
{
  [_functionLocations setObject:[NSNull null] forKey:symbol];
}

-(void) dealloc
{
  RELEASE (_macros);
  RELEASE (_knownMacros);
  RELEASE (_knownSymbolMacros);
  RELEASE (_symbolMacros);
  RELEASE (_goTags);
  RELEASE (_functionLocations);
  RELEASE (_declarations);
  RELEASE (_parent);
  [super dealloc];
}
@end
