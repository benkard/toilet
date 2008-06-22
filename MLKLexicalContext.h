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

#import "MLKLispValue.h"

@class MLKEnvironment, MLKSymbol, NSLinkedList, NSSet,
       NSMutableDictionary, NSString, MLKCons;


@interface MLKLexicalContext : MLKLispValue
{
  MLKEnvironment *_macroBindings;
  MLKEnvironment *_goTags;
  NSMutableDictionary *_variableLocations;
  MLKCons *_declarations;
  MLKLexicalContext *_parent;
}

-(MLKLexicalContext *) initWithParent:(MLKLexicalContext *)aContext
                            variables:(NSSet *)vars
                               goTags:(NSDictionary *)goTags
                               macros:(NSDictionary *)macros
                         declarations:(NSDictionary *)declarations;

+(MLKLexicalContext *) globalContext;
+(MLKLexicalContext *) nullContext;

-(MLKEnvironment *) environment;

-(BOOL) symbolNamesFunction:(MLKSymbol *)symbol;
-(BOOL) symbolNamesMacro:(MLKSymbol *)symbol;

-(id) macroForSymbol:(MLKSymbol *)symbol;
-(id) goTagForSymbol:(MLKSymbol *)symbol;
-(id) variableLocationForSymbol:(MLKSymbol *)symbol;

-(void) addVariable:(MLKSymbol *)symbol;

-(BOOL) variableIsLexical:(MLKSymbol *)symbol;

-(void) dealloc;
@end