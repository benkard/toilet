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

#include <Foundation/NSObject.h>

@class MLKEnvironment, MLKSymbol, NSLinkedList,
       NSMutableDictionary, NSString, NSSet;


@interface MLKLexicalEnvironment : NSObject
{
  MLKEnvironment *_variables;
  MLKEnvironment *_functions;
  MLKLexicalEnvironment *_parent;
}

+(void) initialize;
  
-(MLKLexicalEnvironment *) initWithParent:(MLKLexicalEnvironment *)aContext
                                variables:(NSDictionary *)vars
                                functions:(NSDictionary *)handlers;

+(MLKLexicalEnvironment *) globalEnvironment;

-(NSSet *) variables;
-(NSSet *) functions;

-(id) valueForSymbol:(MLKSymbol *)symbol;
-(void) setValue:(id)value forSymbol:(MLKSymbol *)symbol;
-(void) addValue:(id)value forSymbol:(MLKSymbol *)symbol;
-(void) addBinding:(MLKSymbol *)symbol;

-(id) functionForSymbol:(MLKSymbol *)symbol;
-(void) setFunction:(id)value forSymbol:(MLKSymbol *)symbol;
-(void) addFunction:(id)value forSymbol:(MLKSymbol *)symbol;

-(BOOL) boundp:(MLKSymbol *)symbol;
-(void) makunbound:(MLKSymbol *)symbol;

-(BOOL) fboundp:(MLKSymbol *)symbol;
-(void) fmakunbound:(MLKSymbol *)symbol;

-(void) dealloc;
@end
