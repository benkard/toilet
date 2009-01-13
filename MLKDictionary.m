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

#import "MLKCharacter.h"
#import "MLKCons.h"
#import "MLKDictionary.h"
#import "MLKInteger.h"
#import "functions.h"
#import "globals.h"
#import "util.h"

#import <Foundation/Foundation.h>

#include <stdio.h>
#include <search.h>
#include <string.h>


// http://developer.apple.com/DOCUMENTATION/Cocoa/Reference/Foundation/Miscellaneous/Foundation_Functions/Reference/reference.html#//apple_ref/doc/uid/20000055-BCIGHBEC
static NSMapTableValueCallBacks value_callbacks;

static NSMapTableKeyCallBacks
  eq_callbacks, eql_callbacks, equal_callbacks, equalp_callbacks;


static void retain (NSMapTable *table, const void *x)
{
  id y = (id) x;
  LRETAIN (y);
}

static void release (NSMapTable *table, void *x)
{
  LRELEASE (x);
}

NSString *describe (NSMapTable *table, const void *x)
{
  return MLKPrintToString ((id) x);
}

static BOOL eql (NSMapTable *table, const void *p_x, const void *p_y)
{
  id x = (id)p_x, y = (id)p_y;

  if ((MLKInstanceP (x) && ![x isKindOfClass:[MLKInteger class]])
      || (MLKInstanceP (y) && ![x isKindOfClass:[MLKInteger class]]))
    {
      if ([x isKindOfClass:[MLKCharacter class]])
        return [x isEqual:y];
      else
        return x == y;
    }
  else
    {
      x = MLKCanoniseInteger (x);
      y = MLKCanoniseInteger (y);

      if (MLKFixnumP (x) && MLKFixnumP (y))
        return x == y;
      else if (MLKInstanceP (x) && MLKInstanceP (y))
        return [x isEqual:y];
      else
        return NO;
    }
}

static unsigned int eql_hash (NSMapTable *table, const void *x)
{
  if (MLKInstanceP ((id) x))
    return (unsigned int) x;
  else
    return (unsigned int) MLKCanoniseInteger(x);
}

static BOOL equal (NSMapTable *table, const void *p_x, const void *p_y)
{
  id x = (id)p_x, y = (id)p_y;

  if (MLKFixnumP(x)
      || MLKFixnumP(y)
      || [x isKindOfClass:[MLKNumber class]]
      || [x isKindOfClass:[MLKCharacter class]])
    return eql (table, x, y);
  else if ([x isKindOfClass:[MLKCons class]])
    return ([y isKindOfClass:[MLKCons class]]
            && equal(table, [x car], [y car])
            && equal(table, [x cdr], [y cdr]));
  else if ([x isKindOfClass:[NSString class]])
    return [x isEqual:y];
  // FIXME: Missing cases: pathname, bit vector
  else
    return (x == y);
}

static unsigned int equal_hash (NSMapTable *table, const void *p_x)
{
  id x = (id)p_x;

  if (MLKFixnumP(x))
    return (unsigned int) x;
  else if ([x isKindOfClass:[MLKCons class]]
           || [x isKindOfClass:[NSString class]])
    return [x hash];
  // FIXME: Missing cases: pathname, bit vector
  else
    return (unsigned int) x;
}

static BOOL equalp (NSMapTable *table, const void *p_x, const void *p_y)
{
  // FIXME
  return equal(table, p_x, p_y);
}

static unsigned int equalp_hash (NSMapTable *table, const void *x)
{
  // FIXME
  return 0;
}


@implementation MLKDictionary
+(void) initialize
{
  value_callbacks.retain = retain;
  value_callbacks.release = release;
  value_callbacks.describe = describe;

  eq_callbacks.hash = NULL;
  eq_callbacks.isEqual = NULL;
  eq_callbacks.retain = retain;
  eq_callbacks.release = release;
  eq_callbacks.describe = describe;
  eq_callbacks.notAKeyMarker = MLKEndOfArgumentsMarker;

  eql_callbacks.hash = eql_hash;
  eql_callbacks.isEqual = eql;
  eql_callbacks.retain = retain;
  eql_callbacks.release = release;
  eql_callbacks.describe = describe;
  eql_callbacks.notAKeyMarker = MLKEndOfArgumentsMarker;

  equal_callbacks.hash = equal_hash;
  equal_callbacks.isEqual = equal;
  equal_callbacks.retain = retain;
  equal_callbacks.release = release;
  equal_callbacks.describe = describe;
  equal_callbacks.notAKeyMarker = MLKEndOfArgumentsMarker;

  equalp_callbacks.hash = equalp_hash;
  equalp_callbacks.isEqual = equalp;
  equalp_callbacks.retain = retain;
  equalp_callbacks.release = release;
  equalp_callbacks.describe = describe;
  equalp_callbacks.notAKeyMarker = MLKEndOfArgumentsMarker;
}

+(id) dictionary
{
  return LAUTORELEASE ([[self alloc] init]);
}

-(id) init
{
  return [self initEqlTable];
}

-(id) initWithCapacity:(unsigned int)numItems
{
  m_table = NSCreateMapTable (eql_callbacks, value_callbacks, numItems);
  return self;
}

-(id) initEqTable
{
  m_table = NSCreateMapTable (eq_callbacks, value_callbacks, 0);
  return self;
}

-(id) initEqlTable
{
  m_table = NSCreateMapTable (eql_callbacks, value_callbacks, 0);
  return self;
}

-(id) initEqualTable
{
  m_table = NSCreateMapTable (equal_callbacks, value_callbacks, 0);
  return self;
}

-(id) initEqualPTable
{
  m_table = NSCreateMapTable (equalp_callbacks, value_callbacks, 0);
  return self;
}


-(NSUInteger) count
{
  return NSCountMapTable (m_table);
}

-(NSEnumerator *) keyEnumerator
{
  NSArray *keys = NSAllMapTableKeys (m_table);
  return [keys objectEnumerator];
}

-(id) objectForKey:(id)key
{
  return NSMapGet (m_table, key);
}

-(void) setObject:(id)object forKey:(id)key
{
  NSMapInsert (m_table, key, object);
}

-(void) removeObjectForKey:(id)key
{
  NSMapRemove (m_table, key);
}

-(void) removeAllObjects
{
  NSResetMapTable (m_table);
}

-(NSArray *) allKeys
{
  return NSAllMapTableKeys (m_table);
}

-(NSArray *) allValues
{
  return NSAllMapTableValues (m_table);
}


+(Class) classForKeyedUnarchiver
{
  return [MLKDictionary class];
}

-(Class) classForKeyedArchiver
{
  return [MLKDictionary class];
}


-(void) dealloc
{
  NSFreeMapTable (m_table);
  [super dealloc];
}

-(void) finalize
{
  NSFreeMapTable (m_table);
}
@end
