/* -*- mode: objc; coding: utf-8 -*- */
/* Étoilisp/Mulklisp, a Common Lisp subset for the Étoilé runtime.
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

#import <Foundation/NSArray.h>
#import <Foundation/NSString.h>


@implementation MLKCons
+(MLKCons*) cons:(id)car with:(id)cdr
{
  return AUTORELEASE ([[self alloc] initWithCar:car cdr:cdr]);
}

-(MLKCons*) initWithCar:(id)car cdr:(id)cdr
{
  self = [super init];
  ASSIGN (_car, car);
  ASSIGN (_cdr, cdr);
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
          cons = tail = [MLKCons cons:item with:nil];
        }
      else
        {
          [tail setCdr:[MLKCons cons:item with:nil]];
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
  ASSIGN (_car, value);
}

-(void) setCdr:(id)value
{
  ASSIGN (_cdr, value);
}

-(NSArray *)array
{
  NSMutableArray *array = [NSMutableArray array];
  id rest = self;
  
  while (rest)
    {
      [array addObject:[rest car]];
      rest = [rest cdr];
    }

  return array;
}

-(NSString *)bareDescriptionForLisp
{
  if (!_cdr)
    return [NSString stringWithFormat:@"%@", [_car descriptionForLisp]];
  else if ([_cdr isKindOfClass:[MLKCons class]])
    return [NSString stringWithFormat:@"%@ %@",
                     [_car descriptionForLisp],
                     [_cdr bareDescriptionForLisp]];
  else
    return [NSString stringWithFormat:@"%@ . %@",
                     [_car descriptionForLisp],
                     [_cdr descriptionForLisp]];
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

-(void) dealloc
{
  RELEASE (_car);
  RELEASE (_cdr);
  [super dealloc];
}
@end
