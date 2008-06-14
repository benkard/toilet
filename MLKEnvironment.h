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

@class NSMutableDictionary, MLKSymbol;


@interface MLKEnvironment : MLKLispValue
{
  MLKEnvironment *_parent;
  NSMutableDictionary *_bindings;
}

-(MLKEnvironment *) init;
-(MLKEnvironment *) initWithParent:(MLKEnvironment *)parent;
-(MLKEnvironment *) initWithBindings:(NSDictionary *)bindings;
-(MLKEnvironment *) initWithParent:(MLKEnvironment *)parent bindings:(NSDictionary *)bindings;

-(MLKEnvironment *) parent;

-(void) addBindings:(NSDictionary *)bindings;
-(void) addBinding:(MLKSymbol *)symbol to:(id)value;
-(void) setBinding:(MLKSymbol *)symbol to:(id)value;
-(id) valueForBinding:(MLKSymbol *)symbol;

-(MLKEnvironment *) environmentForBinding:(MLKSymbol *)symbol;

// Private methods.
-(void) setBinding:(MLKSymbol *)symbol to:(id)value inEnvironment:(MLKEnvironment *)env;
-(id) valueForBinding:(MLKSymbol *)symbol inEnvironment:(MLKEnvironment *)env;

-(void) dealloc;
@end
