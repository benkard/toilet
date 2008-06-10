/* -*- mode: objc; coding: utf-8 -*- */
/* Copyright 2008, Matthias Benkard. */

#import "Foundation/NSObject.h"
#import "MLKLispValue.h"

@class MLKCons;


@interface MLKLinkedList : NSObject
{
  MLKCons *_firstCons;
}

-(MLKLinkedList*) init;
-(MLKLinkedList*) initWithCons:(MLKCons*)cons;

-(void) push: (id)object;
-(id) pop;
-(MLKCons*) firstCons;
-(BOOL) null;

#ifdef __OBJC2__
-(NSUInteger) countByEnumeratingWithState:(NSFastEnumerationState *)state objects:(id *)stackbuf count:(NSUInteger)len;
#endif
@end
