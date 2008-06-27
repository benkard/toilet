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

#import "MLKFuncallable.h"
#import "MLKLispValue.h"

#import <Foundation/NSSet.h>

@class MLKEnvironment, MLKLexicalEnvironment, MLKSymbol, NSLinkedList, NSSet,
       NSMutableDictionary, NSString, MLKCons;


@interface MLKLexicalContext : MLKLispValue
{
  NSArray *_knownMacros;
  NSArray *_knownSymbolMacros;
  MLKEnvironment *_macros;
  MLKEnvironment *_symbolMacros;
  MLKEnvironment *_goTags;
  NSMutableSet *_functions;
  NSMutableSet *_variables;
  id _declarations;
  MLKLexicalContext *_parent;
}

+(void) initialize;
  
-(MLKLexicalContext *) initWithParent:(MLKLexicalContext *)aContext
                            variables:(NSSet *)vars
                            functions:(NSSet *)functions
                               goTags:(NSDictionary *)goTags
                               macros:(NSDictionary *)macros
                         symbolMacros:(NSDictionary *)symbolMacros
                         declarations:(id)declarations;

+(MLKLexicalContext *) contextWithParent:(MLKLexicalContext *)aContext
                               variables:(NSSet *)vars
                               functions:(NSSet *)functions
                                  goTags:(NSDictionary *)goTags
                                  macros:(NSDictionary *)macros
                            symbolMacros:(NSDictionary *)symbolMacros
                            declarations:(id)declarations;

+(MLKLexicalContext *) globalContext;

-(BOOL) symbolNamesFunction:(MLKSymbol *)symbol;
-(BOOL) symbolNamesMacro:(MLKSymbol *)symbol;
-(BOOL) symbolNamesSymbolMacro:(MLKSymbol *)symbol;

-(id) macroForSymbol:(MLKSymbol *)symbol;
-(void) setMacro:(id <MLKFuncallable>)function forSymbol:(MLKSymbol *)symbol;
-(void) addMacro:(id <MLKFuncallable>)value forSymbol:(MLKSymbol *)symbol;

-(id) symbolMacroForSymbol:(MLKSymbol *)symbol;
-(void) setSymbolMacro:(id <MLKFuncallable>)function forSymbol:(MLKSymbol *)symbol;

-(id) goTagForSymbol:(MLKSymbol *)symbol;

// FIXME?
//-(MLKLexicalEnvironment *) instantiateWithVariables:(NSDictionary *)variables
//                                          functions:(NSDictionary *)functions;

-(void) addVariable:(MLKSymbol *)symbol;
-(void) addFunction:(MLKSymbol *)symbol;

-(BOOL) variableIsLexical:(MLKSymbol *)symbol;

-(void) dealloc;
@end
