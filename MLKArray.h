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

#import <Foundation/NSArray.h>
#import <Foundation/NSData.h>

#define NSUInteger unsigned int


@interface MLKArray : NSMutableArray
{
  int _fillPointer;
  id *_buffer;
  NSMutableData *_data;
  NSMutableArray *_dimensions;
  NSArray *_displacement;
}

+(id) array;
-(id) initWithDimensions:(NSArray *)dimensions;

-(NSArray *) dimensions;

// The following methods are like the similarly named
// NSArray/NSMutableArray methods but treat nil as just another object.
// Where nil would be returned otherwise, these methods throw an
// exception.
-(id) idAtIndex:(NSUInteger)index;
-(void) insertId:(id)anObject atIndex:(NSUInteger)index;
-(void) replaceIdAtIndex:(NSUInteger)index withId:(id)anObject;
-(void) addId:(id)anObject;

// Methods to support ADJUST-ARRAY.
-(void) setSize:(int)size ofDimension:(int)dimension;

// More stuff.
-(void) setFillPointer:(int)fillPointer;
-(int) fillPointer;

-(void) setDisplacement:(NSArray *)array;
-(NSArray *) displacement;

-(NSUInteger) indexOfObjectIdenticalTo:(id)anObject;
-(NSUInteger) indexOfObjectIdenticalTo:(id)anObject inRange:(NSRange)range;
-(NSUInteger) indexOfObject:(id)anObject;
-(NSUInteger) indexOfObject:(id)anObject inRange:(NSRange)range;

// Must override for NSArray.
-(NSUInteger) count;
-(id) objectAtIndex:(NSUInteger)index;

// Must override for NSMutableArray.
-(void) insertObject:(id)anObject atIndex:(NSUInteger)index;
-(void) removeObjectAtIndex:(NSUInteger)index;
-(void) addObject:(id)anObject;
-(void) removeLastObject;
-(void) replaceObjectAtIndex:(NSUInteger)index withObject:(id)anObject;

-(void) dealloc;
@end
