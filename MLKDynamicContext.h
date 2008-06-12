/* -*- mode: objc; coding: utf-8 -*- */
/* Copyright 2008, Matthias Benkard. */

@class MLKClosure, MLKEnvironment, NSLinkedList, NSMutableDictionary, NSString;


@interface MLKDynamicContext
{
  MLKEnvironment *_conditionHandlers;
  MLKEnvironment *_restarts;
  MLKClosure *_currentConditionHandler;
  MLKEnvironment *_environment;
  MLKDynamicContext *_parent;
}

-(MLKDynamicContext *) initWithParent:(MLKDynamicContext *)aContext
                            variables:(NSDictionary *)vars
                             handlers:(NSDictionary *)handlers
                             restarts:(NSDictionary *)restarts
                       currentHandler:(MLKClosure *)handler;

-(MLKDynamicContext *) pushContext;

+(MLKDynamicContext *) currentContext;
+(MLKDynamicContext *) popContext;
@end
