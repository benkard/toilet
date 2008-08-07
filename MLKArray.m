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

  _size = 1;
  e = [dimensions objectEnumerator];
  while ((el = [e nextObject]))
    {
      el = denullify (el);
      _size *= MLKIntWithInteger (el);
    }

  LASSIGN (_dimensions, [dimensions mutableCopy]);
  _data = calloc (_size, sizeof (id));
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
  if (index > _size || (_fillPointer != -1 && index > _fillPointer))
    [NSException raise:@"NSRangeException"
                 format:@"Array index out of bounds"];

  return _data[index];
}

-(void) insertId:(id)anObject atIndex:(NSUInteger)index
{
  _size++;
  if (_fillPointer != -1)
    _fillPointer++;

  _data = realloc (_data, _size * sizeof (id));
  memmove (_data+index+1, _data+index, _size-index);
  _data[index] = anObject;
}

-(void) removeObjectAtIndex:(NSUInteger)index
{
  _size--;
  if (_fillPointer != -1)
    _fillPointer--;

  memmove (_data+index, _data+index+1, _size-index-1);
  _data = realloc (_data, _size * sizeof (id));
}

-(void) replaceIdAtIndex:(NSUInteger)index withId:(id)anObject
{
  _data[index] = anObject;
}

-(NSUInteger) indexOfObjectIdenticalTo:(id)anObject
{
  return [self indexOfObjectIdenticalTo:anObject
               inRange:NSMakeRange(0, _size)];
}

static int eq (const void *x, const void *y)
{
  return (x == y ? 0 : 1);
}

-(NSUInteger) indexOfObjectIdenticalTo:(id)anObject inRange:(NSRange)range
{
  // FIXME: How to treat [NSNull null]?
  return ((id*)lfind (anObject, _data + range.location, &range.length, sizeof(id), eq)
          - _data);
}

-(NSUInteger) indexOfObject:(id)anObject
{
  return [self indexOfObject:anObject inRange:NSMakeRange(0, _size)];
}

static int equalp (const void *x, const void *y)
{
  // FIXME: Hmm...  What about fixnums?  What about nil?
  return ([(id)x isEqual:(id)y] ? 0 : 1);
}

-(NSUInteger) indexOfObject:(id)anObject inRange:(NSRange)range
{
  // FIXME: How to treat [NSNull null]?
  return ((id*)lfind (anObject, _data + range.location, &range.length, sizeof(id), equalp)
          - _data);
}


-(void) addId:(id)anObject
{
  [self insertId:anObject atIndex:(_fillPointer == -1 ? _size-1 : _fillPointer-1)];
}

-(void) setSize:(int)size ofDimension:(int)dimension
{
  // I'd love to comment the following code, but I can't find the right
  // words to do so.  There's this picture of jumping pointers in
  // parallel in my head.  How to describe it?  I pass.

  int i;
  int old_size;
  id *sourcePointer, *destPointer;
  int subblock_length, old_block_length, new_block_length;
  NSEnumerator *e;
  id el;
  id *old_data;

  old_size = _size;

  subblock_length = 1;
  for (i = dimension + 1; i < [_dimensions count]; i++)
    subblock_length *= [[_dimensions objectAtIndex:i] intValue];

  old_block_length = subblock_length * [[_dimensions objectAtIndex:dimension] intValue];
  new_block_length = subblock_length * size;

  [_dimensions replaceObjectAtIndex:dimension
               withObject:[MLKInteger integerWithInt:size]];

  _size = 1;
  e = [_dimensions objectEnumerator];
  while ((el = [e nextObject]))
    {
      el = denullify (el);
      _size *= MLKIntWithInteger (el);
    }

  old_data = _data;
  _data = calloc (_size, sizeof (id));

  sourcePointer = old_data;
  destPointer = _data;
  while (destPointer < _data + size - 1)
    {
      memmove (destPointer, sourcePointer,
               (old_block_length < new_block_length
                ? old_block_length
                : new_block_length)
               * sizeof(id));
      sourcePointer += old_block_length;
      destPointer += new_block_length;
    }

  free (old_data);
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
  return (_fillPointer == -1 ? _size : _fillPointer);
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
    [self removeObjectAtIndex:(_size-1)];
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
  free (_data);
  LDESTROY (_dimensions);
  LDESTROY (_displacement);
  [super dealloc];
}
@end
