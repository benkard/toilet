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

@class MLKSymbol, NSMutableDictionary, NSMutableSet, NSSet, NSString;


@interface MLKPackage : MLKLispValue
{
  NSMutableDictionary *symbols;
  NSMutableSet *exported_symbols;
  NSMutableSet *shadowed_symbols;
  NSMutableSet *nicknames;
  NSString *name;
}

-(MLKPackage *) initWithName:(NSString *)name
                   nicknames:(NSSet *)nicknames;

-(void) usePackage:(MLKPackage *)aPackage;
-(void) import:(MLKSymbol *)aSymbol;
-(void) shadow:(MLKPackage *)aPackage;
-(void) unintern:(MLKSymbol *)aSymbol;
-(MLKSymbol *) intern:(NSString*)symbolName;
-(MLKSymbol *) findSymbol:(NSString*)symbolName;

-(NSString *) name;
-(NSSet *) nicknames;
-(NSSet *) exportedSymbols;
-(NSSet *) shadowedSymbols;
-(NSSet *) allSymbols;

// [clUser cons: @"Mulk", [clUser __intern: @"NIL"]] ought to do the
// Right Thing.
-(void) forwardInvocation:(NSInvocation *)anInvocation;
-(BOOL) respondsToSelector:(SEL)selector;
@end
