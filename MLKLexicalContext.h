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

#import "MLKFuncallable.h"

#import <Foundation/NSArray.h>
#import <Foundation/NSDictionary.h>
#import <Foundation/NSSet.h>

@class MLKEnvironment, MLKLexicalEnvironment, MLKSymbol, NSSet,
       NSMutableDictionary, NSString, MLKCons;


@interface MLKLexicalContext : NSObject
{
  NSMutableSet *_knownMacros;
  NSMutableSet *_knownCompilerMacros;
  NSMutableSet *_knownSymbolMacros;
  MLKEnvironment *_macros;
  MLKEnvironment *_compilerMacros;
  MLKEnvironment *_symbolMacros;
  MLKEnvironment *_goTags;
  NSMutableSet *_functions;
  NSMutableSet *_variables;
  NSMutableDictionary *_functionInfo;
  NSMutableDictionary *_variableInfo;
  id _declarations;
  MLKLexicalContext *_parent;
}

+(void) initialize;
  
-(MLKLexicalContext *) initWithParent:(MLKLexicalContext *)aContext
                            variables:(NSSet *)vars
                            functions:(NSSet *)functions
                               goTags:(NSDictionary *)goTags
                               macros:(NSDictionary *)macros
                       compilerMacros:(NSDictionary *)compilerMacros
                         symbolMacros:(NSDictionary *)symbolMacros
                         declarations:(id)declarations;

+(MLKLexicalContext *) contextWithParent:(MLKLexicalContext *)aContext
                               variables:(NSSet *)vars
                               functions:(NSSet *)functions
                                  goTags:(NSDictionary *)goTags
                                  macros:(NSDictionary *)macros
                          compilerMacros:(NSDictionary *)compilerMacros
                            symbolMacros:(NSDictionary *)symbolMacros
                            declarations:(id)declarations;

+(MLKLexicalContext *) globalContext;

-(BOOL) symbolNamesFunction:(MLKSymbol *)symbol;
-(BOOL) symbolNamesMacro:(MLKSymbol *)symbol;
-(BOOL) symbolNamesSymbolMacro:(MLKSymbol *)symbol;

-(id <MLKFuncallable>) macroForSymbol:(MLKSymbol *)symbol;
-(void) setMacro:(id <MLKFuncallable>)function forSymbol:(MLKSymbol *)symbol;
-(void) addMacro:(id <MLKFuncallable>)value forSymbol:(MLKSymbol *)symbol;

-(id <MLKFuncallable>) compilerMacroForSymbol:(MLKSymbol *)symbol;
-(void) setCompilerMacro:(id <MLKFuncallable>)value forSymbol:(MLKSymbol *)symbol;
-(void) addCompilerMacro:(id <MLKFuncallable>)value forSymbol:(MLKSymbol *)symbol;

-(id <MLKFuncallable>) symbolMacroForSymbol:(MLKSymbol *)symbol;
-(void) setSymbolMacro:(id <MLKFuncallable>)function forSymbol:(MLKSymbol *)symbol;
-(void) addSymbolMacro:(id <MLKFuncallable>)value forSymbol:(MLKSymbol *)symbol;

-(id) goTagForSymbol:(MLKSymbol *)symbol;

-(id) declarations;
-(void) addDeclaration:(id)declaration;

// FIXME?
//-(MLKLexicalEnvironment *) instantiateWithVariables:(NSDictionary *)variables
//                                          functions:(NSDictionary *)functions;

-(void) addVariable:(MLKSymbol *)symbol;
-(void) addFunction:(MLKSymbol *)symbol;

-(BOOL) variableIsLexical:(MLKSymbol *)symbol;

-(id) deepPropertyForVariable:(id)name key:(id)key;
-(void) setDeepProperty:(id)object
            forVariable:(id)name
                    key:(id)key;

-(id) deepPropertyForFunction:(id)name key:(id)key;
-(void) setDeepProperty:(id)object
            forFunction:(id)name
                    key:(id)key;

-(void *) functionCellForSymbol:(id)name;
-(void *) closureDataPointerForSymbol:(id)name;
-(id) bindingForSymbol:(id)name;

-(void) dealloc;
@end
