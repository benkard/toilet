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

@class MLKClosure, MLKEnvironment, MLKSymbol, NSLinkedList,
       NSMutableDictionary, NSString;


@interface MLKDynamicContext : NSObject
{
  MLKEnvironment *_conditionHandlers;
  MLKEnvironment *_restarts;
  MLKEnvironment *_catchTags;
  MLKEnvironment *_environment;
  MLKEnvironment *_activeHandlerEnvironment;  // needed for the Condition Firewall
  MLKDynamicContext *_parent;
}

-(MLKDynamicContext *) initWithParent:(MLKDynamicContext *)aContext
                            variables:(NSDictionary *)vars
                             handlers:(NSDictionary *)handlers
                             restarts:(NSDictionary *)restarts
                            catchTags:(NSDictionary *)catchTags
             activeHandlerEnvironment:(MLKEnvironment *)handlerEnv;

-(MLKDynamicContext *) pushContext;

+(MLKDynamicContext *) currentContext;
+(MLKDynamicContext *) popContext;

-(MLKEnvironment *) environment;

-(id) findRestart:(MLKSymbol *)symbol;
-(id) findHandler:(MLKSymbol *)symbol;
-(id) findCatchTag:(MLKSymbol *)symbol;
-(id) valueForBinding:(MLKSymbol *)symbol;
-(void) setValue:(id)value forBinding:(MLKSymbol *)symbol;
-(void) addValue:(id)value forBinding:(MLKSymbol *)symbol;

-(void) dealloc;
@end
