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
#import <Foundation/NSSet.h>
#import <Foundation/NSString.h>

@class MLKSymbol;


@interface MLKPackage : NSObject
{
  NSMutableDictionary *_accessible_symbols;
  NSMutableSet *_present_symbols;
  //  NSMutableSet *_inherited_symbols;
  NSMutableSet *_exported_symbols;
  NSMutableSet *_shadowing_symbols;
  NSMutableSet *_nicknames;
  NSMutableArray *_used_packages;
  NSMutableArray *_using_packages;
  NSString *_name;
}

+(void) initialize;
  
-(MLKPackage *) initWithName:(NSString *)name
                   nicknames:(NSSet *)nicknames;

+(MLKPackage *) packageWithName:(NSString *)name
                      nicknames:(NSSet *)nicknames;

+(MLKPackage *) findPackage:(NSString *)name;

-(void) usePackage:(MLKPackage *)package;
-(void) unusePackage:(MLKPackage *)package;
-(void) import:(MLKSymbol *)symbol;
-(void) inherit:(MLKSymbol *)symbol;
-(void) uninherit:(MLKSymbol *)symbol;
-(void) export:(MLKSymbol *)symbol;
-(void) unexport:(MLKSymbol *)symbol;
-(void) shadow:(NSString *)symbolName;
-(void) unintern:(MLKSymbol *)aSymbol;
-(MLKSymbol *) intern:(NSString*)symbolName;
-(MLKSymbol *) findSymbol:(NSString*)symbolName;

-(NSString *) name;
-(NSSet *) nicknames;
-(NSSet *) exportedSymbols;
-(NSSet *) shadowingSymbols;
-(NSSet *) allSymbols;
-(NSArray *) usedPackages;
-(NSArray *) usingPackages;

-(NSString *) descriptionForLisp;

-(void) dealloc;
@end
