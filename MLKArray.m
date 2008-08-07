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


#import "MLKArray.h"
#import "functions.h"
#import "util.h"

#import <Foundation/NSArray.h>
#import <Foundation/NSEnumerator.h>

#include <stdio.h>
#include <search.h>


@implementation MLKArray
-(id) initWithDimensions:(NSArray *)dimensions
{
  NSEnumerator *e;
  id el;
  int size;

  size = 1;
  e = [dimensions objectEnumerator];
  while ((el = [e nextObject]))
    {
      el = denullify (el);
      size *= MLKIntWithInteger (el);
    }

  LASSIGN (_dimensions, [dimensions mutableCopy]);
  _data = [[NSMutableData alloc]
            initWithLength:(size * sizeof(id))];
  _fillPointer = -1;
  _displacement = nil;

  return self;
}

-(NSArray *) dimensions
{
  return _dimensions;
}

-(id) idAtIndex:(NSUInteger)index
{
  if (index > [_data length] || (_fillPointer != -1 && index > _fillPointer))
    [NSException raise:@"NSRangeException"
                 format:@"Array index out of bounds"];

  return ((id*)[_data bytes])[index];
}

-(void) insertId:(id)anObject atIndex:(NSUInteger)index
{
  id *buffer;
  int size;

  if (_fillPointer != -1)
    _fillPointer++;

  [_data increaseLengthBy:sizeof(id)];
  size = [_data length];
  buffer = [_data mutableBytes];

  memmove (buffer+index+1, buffer+index, size - index*sizeof(id));
  buffer[index] = anObject;
}

-(void) removeObjectAtIndex:(NSUInteger)index
{
  id *buffer;
  int size;

  if (_fillPointer != -1)
    _fillPointer--;

  buffer = [_data mutableBytes];
  size = [_data length];

  memmove (buffer+index, buffer+index+1, size - (index+1)*sizeof(id));
  [_data setLength:((size-1) * sizeof(id))];
}

-(void) replaceIdAtIndex:(NSUInteger)index withId:(id)anObject
{
  ((id*)[_data mutableBytes])[index] = anObject;
}

-(NSUInteger) indexOfObjectIdenticalTo:(id)anObject
{
  return [self indexOfObjectIdenticalTo:anObject
               inRange:NSMakeRange(0, [_data length])];
}

static int eq (const void *x, const void *y)
{
  return (x == y ? 0 : 1);
}

-(NSUInteger) indexOfObjectIdenticalTo:(id)anObject inRange:(NSRange)range
{
  // FIXME: How to treat [NSNull null]?
  const id *buffer = [_data bytes];
  return ((id*)lfind (anObject, buffer + range.location, &range.length, sizeof(id), eq)
          - buffer) / sizeof(id);
}

-(NSUInteger) indexOfObject:(id)anObject
{
  return [self indexOfObject:anObject inRange:NSMakeRange(0, [_data length])];
}

static int equalp (const void *x, const void *y)
{
  // FIXME: Hmm...  What about fixnums?  What about nil?
  return ([(id)x isEqual:(id)y] ? 0 : 1);
}

-(NSUInteger) indexOfObject:(id)anObject inRange:(NSRange)range
{
  // FIXME: How to treat [NSNull null]?
  const id *buffer = [_data bytes];
  return ((id*)lfind (anObject, buffer + range.location, &range.length, sizeof(id), equalp)
          - buffer) / sizeof(id);
}


-(void) addId:(id)anObject
{
  [self insertId:anObject
        atIndex:(_fillPointer == -1
                 ? ([_data length]/sizeof(id))-1
                 : _fillPointer-1)];
}

-(void) setSize:(int)size ofDimension:(int)dimension
{
  // I'd love to comment the following code, but I can't find the right
  // words to do so.  There's this picture of jumping pointers in
  // parallel in my head.  How to describe it?  I pass.

  int i;
  int new_size;
  const id *sourcePointer;
  id *destPointer;
  int subblock_length, old_block_length, new_block_length;
  NSEnumerator *e;
  id el;
  NSMutableData *old_data;
  id *buffer;
  const id *old_buffer;

  subblock_length = 1;
  for (i = dimension + 1; i < [_dimensions count]; i++)
    subblock_length *= [[_dimensions objectAtIndex:i] intValue];

  old_block_length = subblock_length * [[_dimensions objectAtIndex:dimension] intValue];
  new_block_length = subblock_length * size;

  [_dimensions replaceObjectAtIndex:dimension
               withObject:[MLKInteger integerWithInt:size]];

  new_size = 1;
  e = [_dimensions objectEnumerator];
  while ((el = [e nextObject]))
    {
      el = denullify (el);
      new_size *= MLKIntWithInteger (el);
    }

  old_data = _data;
  _data = [[NSMutableData alloc]
            initWithLength:(new_size * sizeof(id))];

  old_buffer = [old_data bytes];
  buffer = [_data mutableBytes];

  sourcePointer = old_buffer;
  destPointer = buffer;
  while (destPointer < buffer + (new_size/sizeof(id)) - 1)
    {
      memmove (destPointer, sourcePointer,
               (old_block_length < new_block_length
                ? old_block_length
                : new_block_length)
               * sizeof(id));
      sourcePointer += old_block_length;
      destPointer += new_block_length;
    }

   LDESTROY (old_data);
}

-(void) setFillPointer:(int)fillPointer
{
  _fillPointer = fillPointer;
}

-(int) fillPointer
{
  return _fillPointer;
}

-(void) setDisplacement:(NSArray *)array
{
  LASSIGN (_displacement, array);
}

-(NSArray *) displacement
{
  return _displacement;
}

-(NSUInteger) count
{
  return (_fillPointer == -1 ? [_data length]/sizeof(id) : _fillPointer);
}

-(id) objectAtIndex:(NSUInteger)index
{
  NS_DURING
    {
      NS_VALUERETURN (nullify([self idAtIndex:index]), id);
    }
  NS_HANDLER
    {
      if ([[localException name] isEqualToString:@"NSRangeException"])
        return nil;
      else
        [localException raise];
      return nil;
    }
  NS_ENDHANDLER;
}

-(void) insertObject:(id)anObject atIndex:(NSUInteger)index
{
  [self insertId:denullify(anObject) atIndex:index];
}

-(void) addObject:(id)anObject
{
  [self addId:denullify(anObject)];  
}

-(void) removeLastObject
{
  if (_fillPointer == -1)
    [self removeObjectAtIndex:([_data length]/sizeof(id) - 1)];
  else if (_fillPointer == 0)
    [NSException raise:@"NSRangeException"
                 format:@"Tried to remove an object from an empty array"];
  else
    _fillPointer--;
}

-(void) replaceObjectAtIndex:(NSUInteger)index withObject:(id)anObject
{
  [self replaceIdAtIndex:index withId:anObject];  
}

-(void) dealloc
{
  LDESTROY (_data);
  LDESTROY (_dimensions);
  LDESTROY (_displacement);
  [super dealloc];
}
@end
