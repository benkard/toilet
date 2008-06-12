/* -*- mode: objc; coding: utf-8 -*- */
/* Copyright 2008, Matthias Benkard. */

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
   : (id) (parent ? (id) parent_member : nil));


@implementation MLKDynamicContext
-(MLKDynamicContext *) initWithParent:(MLKDynamicContext *)aContext
                            variables:(NSDictionary *)vars
                             handlers:(NSDictionary *)handlers
                             restarts:(NSDictionary *)restarts
                       currentHandler:(MLKClosure *)handler
{
  _parent = (aContext ? aContext : [MLKDynamicContext currentContext]);
  _environment = MAKE_ENVIRONMENT(vars, _parent, _parent->_environment);
  _conditionHandlers = MAKE_ENVIRONMENT(handlers,
                                        _parent,
                                        _parent->_conditionHandlers);
  _restarts = MAKE_ENVIRONMENT(restarts, _parent, _parent->_restarts);
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
@end
