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

#import "MLKCons.h"
#import "MLKPackage.h"
#import "runtime-compatibility.h"
#import "util.h"

#import <Foundation/NSArray.h>
#import <Foundation/NSString.h>


@implementation MLKCons
+(MLKCons*) cons:(id)car with:(id)cdr
{
  return LAUTORELEASE ([[self alloc] initWithCar:car cdr:cdr]);
}

-(MLKCons*) initWithCar:(id)car cdr:(id)cdr
{
  self = [super init];
  LASSIGN (_car, car);
  LASSIGN (_cdr, cdr);
  return self;
}

+(MLKCons*) listWithArray:(NSArray *)array
{
  MLKCons *cons, *tail;
  int i;

  cons = nil;
  tail = nil;

  for (i = 0; i < [array count]; i++)
    {
      id item = [array objectAtIndex:i];
      if (!tail)
        {
          cons = tail = [MLKCons cons:denullify(item) with:nil];
        }
      else
        {
          [tail setCdr:[MLKCons cons:denullify(item) with:nil]];
          tail = [tail cdr];
        }
    }

  return cons;
}

-(id) car
{
  return _car;
}

-(id) cdr
{
  return _cdr;
}

-(void) setCar:(id)value
{
  LASSIGN (_car, value);
}

-(void) setCdr:(id)value
{
  LASSIGN (_cdr, value);
}

-(NSArray *)array
{
  NSMutableArray *array = [NSMutableArray array];
  id rest = self;
  
  while (rest)
    {
      [array addObject:nullify([rest car])];
      rest = [rest cdr];
    }

  return array;
}

-(void) appendObject:(id)object
{
  MLKCons *rest;

  rest = self;
  while (rest->_cdr)
    {
      rest = rest->_cdr;
    }

  LASSIGN (rest->_cdr, object);
}

-(MLKCons *) listByAppendingObject:(id)object
{
  MLKCons *rest = _cdr;
  MLKCons *new_list = [MLKCons cons:_car with:nil];
  MLKCons *tail = new_list;

  while (rest)
    {
      LASSIGN (tail->_cdr, [MLKCons cons:rest->_car with:nil]);
      tail = tail->_cdr;
    }

  LASSIGN (tail->_cdr, object);

  return new_list;
}

-(MLKCons *) copyList
{
  return [self listByAppendingObject:nil];
}

-(NSString *) bareDescriptionForLisp
{
  if (!_cdr)
    return [NSString stringWithFormat:@"%@",
                     MLKPrintToString(_car)];
  else if ([_cdr isKindOfClass:[MLKCons class]])
    return [NSString stringWithFormat:@"%@ %@",
                     MLKPrintToString(_car),
                     [_cdr bareDescriptionForLisp]];
  else
    return [NSString stringWithFormat:@"%@ . %@",
                     MLKPrintToString(_car),
                     MLKPrintToString(_cdr)];
}

-(NSString *)descriptionForLisp
{
  if ([_cdr isKindOfClass:[MLKCons class]])
    {
      if (_car == [[MLKPackage findPackage:@"COMMON-LISP"] intern:@"QUOTE"])
        return [NSString stringWithFormat:@"'%@", [_cdr bareDescriptionForLisp]];
      else if (_car == [[MLKPackage findPackage:@"TOILET-SYSTEM"] intern:@"QUASIQUOTE"])
        return [NSString stringWithFormat:@"`%@", [_cdr bareDescriptionForLisp]];
      else if (_car == [[MLKPackage findPackage:@"TOILET-SYSTEM"] intern:@"UNQUOTE"])
        return [NSString stringWithFormat:@",%@", [_cdr bareDescriptionForLisp]];
      else if (_car == [[MLKPackage findPackage:@"TOILET-SYSTEM"] intern:@"UNQUOTE-SPLICING"])
        return [NSString stringWithFormat:@",@%@", [_cdr bareDescriptionForLisp]];
    }

    return [NSString stringWithFormat:@"(%@)", [self bareDescriptionForLisp]];
}

-(BOOL) isEqual:(id)object
{
  if ([object isKindOfClass:[MLKCons class]])
    return ([((MLKCons*)object)->_car isEqual:_car]
            && [((MLKCons*)object)->_cdr isEqual:_cdr]);
  else
    return NO;
}

-(id) copyWithZone:(NSZone *)zone
{
  MLKCons *copy = [MLKCons allocWithZone:zone];
  LASSIGN (copy->_car, _car);
  LASSIGN (copy->_cdr, _cdr);
  return copy;
}

-(void) dealloc
{
  LRELEASE (_car);
  LRELEASE (_cdr);
  [super dealloc];
}
@end
