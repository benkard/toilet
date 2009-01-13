/* -*- mode: objc; coding: utf-8 -*- */
/* Toilet Lisp, a Common Lisp subset for the Étoilé runtime.
 * Copyright (C) 2008, 2009  Matthias Andreas Benkard.
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

#import <Foundation/Foundation.h>

#define NSUInteger unsigned int


@interface MLKDictionary : NSMutableDictionary
{
  NSMapTable *m_table;
}

+(void) initialize;

+(id) dictionary;

-(id) init;
-(id) initWithCapacity:(unsigned int)numItems;
-(id) initEqTable;
-(id) initEqlTable;
-(id) initEqualTable;
-(id) initEqualPTable;
//-(id) initWithPredicate:(id <MLKFuncallable>)predicate
//      hash:(id <MLKFuncallable>)hash;


-(NSUInteger) count;
-(NSEnumerator *) keyEnumerator;
-(id) objectForKey:(id)key;
-(void) setObject:(id)object forKey:(id)key;
-(void) removeObjectForKey:(id)key;
-(void) removeAllObjects;
-(NSArray *) allKeys;
-(NSArray *) allValues;

-(void) dealloc;
-(void) finalize;
@end
