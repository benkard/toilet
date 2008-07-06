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

#import "MLKBinding.h"
#import "MLKLispValue.h"
#import "MLKSymbol.h"

#import <Foundation/NSDictionary.h>
#import <Foundation/NSSet.h>
#import <Foundation/NSString.h>


@interface MLKEnvironment : MLKLispValue
{
  MLKEnvironment *_parent;
  NSMutableDictionary *_bindings;
}

-(MLKEnvironment *) init;
-(MLKEnvironment *) initWithParent:(MLKEnvironment *)parent;
-(MLKEnvironment *) initWithValues:(NSDictionary *)bindings;
-(MLKEnvironment *) initWithParent:(MLKEnvironment *)parent
                            values:(NSDictionary *)bindings;

-(MLKEnvironment *) parent;

-(NSSet *) bindings;
-(void) addBinding:(MLKBinding *)binding forSymbol:(MLKSymbol *)symbol;
-(void) setBinding:(MLKBinding *)binding forSymbol:(MLKSymbol *)symbol;
-(void) addBindingForSymbol:(MLKSymbol *)symbol;
-(void) addBindings:(NSDictionary *)bindings;
-(void) addValues:(NSDictionary *)bindings;
-(void) addValue:(id)value forSymbol:(MLKSymbol *)symbol;
-(void) setValue:(id)value forSymbol:(MLKSymbol *)symbol;
-(MLKBinding *) bindingForSymbol:(MLKSymbol *)symbol;
-(id) valueForSymbol:(MLKSymbol *)symbol;

-(MLKEnvironment *) environmentForSymbol:(MLKSymbol *)symbol;

-(BOOL) boundp:(MLKSymbol *)symbol;
-(void) makunbound:(MLKSymbol *)symbol;

-(void) dealloc;
@end
