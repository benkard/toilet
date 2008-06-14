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

#import <Foundation/NSDictionary.h>
#import <Foundation/NSArray.h>
#import <Foundation/NSThread.h>

#import "MLKDynamicContext.h"
#import "MLKEnvironment.h"
#import "MLKLinkedList.h"


#define MAKE_ENVIRONMENT(variable, parent, parent_member)               \
  (variable                                                             \
   ? (id) [[MLKEnvironment alloc]                                       \
            initWithParent:(parent                                      \
                            ? (id) parent_member                        \
                            : nil)                                      \
                  bindings:vars]                                        \
   : (id) (parent ? (id) RETAIN (parent_member) : nil));


@implementation MLKDynamicContext
-(MLKDynamicContext *) initWithParent:(MLKDynamicContext *)aContext
                            variables:(NSDictionary *)vars
                             handlers:(NSDictionary *)handlers
                             restarts:(NSDictionary *)restarts
                            catchTags:(NSDictionary *)catchTags
                       currentHandler:(MLKClosure *)handler
{
  self = [super init];
  ASSIGN (_parent, (aContext ? aContext : [MLKDynamicContext currentContext]));
  _environment = MAKE_ENVIRONMENT(vars, _parent, _parent->_environment);
  _conditionHandlers = MAKE_ENVIRONMENT(handlers,
                                        _parent,
                                        _parent->_conditionHandlers);
  _restarts = MAKE_ENVIRONMENT(restarts, _parent, _parent->_restarts);
  _catchTags = MAKE_ENVIRONMENT(catchTags, _parent, _parent->_catchTags);
  _currentConditionHandler = (handler
                              ? (id) handler
                              : (_parent
                                 ? (id) _parent->_currentConditionHandler
                                 : nil));
  return self;
}

-(MLKDynamicContext *) pushContext
{
  return [[[NSThread currentThread] threadDictionary]
           objectForKey:@"MLKDynamicContext"];
}

+(MLKDynamicContext *) currentContext
{
  return [[[NSThread currentThread] threadDictionary]
           objectForKey:@"MLKDynamicContext"];
}

+(MLKDynamicContext *) popContext
{
  MLKDynamicContext *context = [self currentContext];
  [[[NSThread currentThread] threadDictionary] setObject:context->_parent
                                               forKey:@"MLKDynamicContext"];
  return context;
}

-(MLKEnvironment *) environment
{
  return _environment;
}

-(id) findRestart:(MLKSymbol *)symbol
{
  return [_restarts valueForBinding:symbol];
}

/*
-(id) findHandler:(MLKSymbol *)symbol
{
  // ???
}
*/

-(id) findCatchTag:(MLKSymbol *)symbol
{
  return [_catchTags valueForBinding:symbol];
}

-(void) dealloc
{
  RELEASE (_conditionHandlers);
  RELEASE (_restarts);
  RELEASE (_catchTags);
  RELEASE (_currentConditionHandler);
  RELEASE (_environment);
  RELEASE (_parent);
  [super dealloc];
}
@end
