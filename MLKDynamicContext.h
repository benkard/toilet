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

#import "MLKBinding.h"
#import "MLKEnvironment.h"
#import "MLKSymbol.h"

#import <Foundation/NSObject.h>
#import <Foundation/NSDictionary.h>
#import <Foundation/NSSet.h>
#import <Foundation/NSString.h>


@interface MLKDynamicContext : NSObject
{
  MLKEnvironment *_conditionHandlers;
  MLKEnvironment *_restarts;
  NSSet *_catchTags;
  MLKEnvironment *_environment;
  MLKEnvironment *_activeHandlerEnvironment;  // needed for the Condition Firewall
  MLKDynamicContext *_parent;
}

+(void) initialize;

-(MLKDynamicContext *) initWithParent:(MLKDynamicContext *)aContext
                            variables:(NSDictionary *)vars
                             handlers:(NSDictionary *)handlers
                             restarts:(NSDictionary *)restarts
                            catchTags:(NSSet *)catchTags
             activeHandlerEnvironment:(MLKEnvironment *)handlerEnv;

+(MLKDynamicContext *) globalContext;

-(MLKDynamicContext *) pushContext;

+(MLKDynamicContext *) currentContext;
+(MLKDynamicContext *) popContext;

-(MLKEnvironment *) environment;

-(id) findRestart:(MLKSymbol *)symbol;
-(id) findHandler:(MLKSymbol *)symbol;

-(BOOL) catchTagIsEstablished:(id)tag;

-(id) valueForSymbol:(MLKSymbol *)symbol;
-(void) setValue:(id)value forSymbol:(MLKSymbol *)symbol;
-(void) addValue:(id)value forSymbol:(MLKSymbol *)symbol;
-(void) addBindingForSymbol:(MLKSymbol *)symbol;
-(MLKBinding *) bindingForSymbol:(MLKSymbol *)symbol;

-(BOOL) boundp:(MLKSymbol *)symbol;
-(void) makunbound:(MLKSymbol *)symbol;

-(void) dealloc;
@end
