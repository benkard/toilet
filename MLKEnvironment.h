/* -*- mode: objc; coding: utf-8 -*- */
/* Copyright 2008, Matthias Benkard. */

#import "MLKLispValue.h"

@class NSMutableDictionary, MLKLinkedList, MLKSymbol;


@interface MLKEnvironment : MLKLispValue
{
  MLKLinkedList *_bindings;
}

-(MLKEnvironment *) init;
-(MLKEnvironment *) initWithParent:(MLKEnvironment *)parent;

-(void) setBinding:(MLKSymbol *)symbol to:(id)value;
-(id) valueForBinding:(MLKSymbol *)symbol;
@end
