/* -*- mode: objc; coding: utf-8 -*- */
/* Copyright 2008, Matthias Benkard. */

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

-(void) addBindings:(NSDictionary *)bindings;
-(void) addBinding:(MLKSymbol *)symbol to:(id)value;
-(void) setBinding:(MLKSymbol *)symbol to:(id)value;
-(id) valueForBinding:(MLKSymbol *)symbol;

// Private methods.
-(void) setBinding:(MLKSymbol *)symbol to:(id)value inEnvironment:(MLKEnvironment *)env;
-(id) valueForBinding:(MLKSymbol *)symbol inEnvironment:(MLKEnvironment *)env;
@end
