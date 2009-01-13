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

#import "MLKDictionary.h"
#import "functions.h"
#import "globals.h"
#import "util.h"

#import <Foundation/Foundation.h>

#include <stdio.h>
#include <search.h>
#include <string.h>


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
    return x == y;
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

static unsigned eql_hash (NSMapTable *table, const void *x)
{
  if (MLKInstanceP ((id) x))
    return (unsigned) x;
  else
    return (unsigned) MLKCanoniseInteger(x);
}

static BOOL equal (NSMapTable *table, const void *x, const void *y)
{
  // FIXME
}

static unsigned equal_hash (NSMapTable *table, const void *x)
{
  // FIXME
}

static BOOL equalp (NSMapTable *table, const void *x, const void *y)
{
  // FIXME
}

static unsigned equalp_hash (NSMapTable *table, const void *x)
{
  // FIXME
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

-(id) initEqlTable
{
  m_table = NSCreateMapTable (eql_callbacks, value_callbacks, 0);
  return self;
}

-(void) dealloc
{
  NSFreeMapTable (m_table);
}

-(void) finalize
{
  NSFreeMapTable (m_table);
}
@end
