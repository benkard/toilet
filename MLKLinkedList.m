/* -*- mode: objc; coding: utf-8 -*- */
/* Copyright 2008, Matthias Benkard. */

#import "MLKLinkedList.h"
#import "MLKCons.h"


@implementation MLKLinkedList
-(MLKLinkedList*) init
{
  self = [super init];
  _firstCons = nil;
  return self;
}

-(MLKLinkedList*) initWithCons:(MLKCons*)cons
{
  self = [super init];
  ASSIGN (_firstCons, cons);
  return self;
}

-(void) push: (id)object
{
  ASSIGN (_firstCons, [MLKCons cons:object with:_firstCons]);
}

-(id) pop
{
  id retval = [_firstCons car];
  RETAIN (retval);
  ASSIGN (_firstCons, [_firstCons cdr]);
  AUTORELEASE (retval);
  return retval;
}

-(MLKCons*) firstCons
{
  return _firstCons;
}

-(BOOL) null
{
  return !_firstCons;
}

#ifdef __OBJC2__
-(NSUInteger) countByEnumeratingWithState:(NSFastEnumerationState *)state objects:(id *)stackbuf count:(NSUInteger)len
{
  if (state->state == 0)
    {
      state->mutationsPtr = &_firstCons;
      state->extra[0] = (unsigned long) &_firstCons;
    }
  
  MLKCons *currentCons = (MLKCons*) state->extra[0];
  if (currentCons)
    {
      state->itemsPtr = [currentCons car];
      return 1;
    }
  else
    return 0;
}
#endif

-(void) dealloc
{
  RELEASE (_firstCons);
  [super dealloc];
}
@end
