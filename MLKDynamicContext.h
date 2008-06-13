/* -*- mode: objc; coding: utf-8 -*- */
/* Copyright 2008, Matthias Benkard. */

@class MLKClosure, MLKEnvironment, NSLinkedList, NSMutableDictionary, NSString;


@interface MLKDynamicContext : NSObject
{
  MLKEnvironment *_conditionHandlers;
  MLKEnvironment *_restarts;
  MLKEnvironment *_catchTags;
  MLKClosure *_currentConditionHandler;  // needed for the Condition Firewall
  MLKEnvironment *_environment;
  MLKDynamicContext *_parent;
}

-(MLKDynamicContext *) initWithParent:(MLKDynamicContext *)aContext
                            variables:(NSDictionary *)vars
                             handlers:(NSDictionary *)handlers
                             restarts:(NSDictionary *)restarts
                            catchTags:(NSDictionary *)catchTags
                       currentHandler:(MLKClosure *)handler;

-(MLKDynamicContext *) pushContext;

+(MLKDynamicContext *) currentContext;
+(MLKDynamicContext *) popContext;

-(void) dealloc;
@end
