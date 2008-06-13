/* Étoilisp, a Common Lisp subset for Étoilé.
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
                           
#include <UnitKit/UnitKit.h>
#include <Foundation/Foundation.h>

#include "MLKCons.h"
#include "MLKDynamicContext.h"
#include "MLKEnvironment.h"
#include "MLKLinkedList.h"
#include "MLKSymbol.h"

@interface MLKLowLevelTests : NSObject <UKTest>
@end


@implementation MLKLowLevelTests
-(id) initForTest
{
  return self;
}


-(id) testCons
{
  id obj1 = @"Mulk.";
  id obj2 = AUTORELEASE ([[NSMutableDictionary alloc] init]);

  MLKCons *cons2 = [MLKCons cons:obj1 with:obj2];
  MLKCons *cons3 = [MLKCons cons:obj1 with:nil];
  MLKCons *cons4 = [MLKCons cons:nil with:nil];
  MLKCons *cons5 = [MLKCons cons:nil with:obj2];

  MLKCons *cons6 = AUTORELEASE ([[MLKCons alloc] initWithCar:obj1 cdr:obj2]);
  MLKCons *cons7 = AUTORELEASE ([[MLKCons alloc] initWithCar:obj1 cdr:nil]);
  MLKCons *cons8 = AUTORELEASE ([[MLKCons alloc] initWithCar:nil cdr:nil]);
  MLKCons *cons9 = AUTORELEASE ([[MLKCons alloc] initWithCar:nil cdr:obj2]);

  UKTrue ([cons2 car] == obj1);
  UKTrue ([cons3 car] == obj1);
  UKFalse ([cons4 car] == obj1);
  UKFalse ([cons5 car] == obj1);
  UKTrue ([cons6 car] == obj1);
  UKTrue ([cons7 car] == obj1);
  UKFalse ([cons8 car] == obj1);
  UKFalse ([cons9 car] == obj1);

  UKTrue ([cons2 cdr] == obj2);
  UKFalse ([cons3 cdr] == obj2);
  UKFalse ([cons4 cdr] == obj2);
  UKTrue ([cons5 cdr] == obj2);
  UKTrue ([cons6 cdr] == obj2);
  UKFalse ([cons7 cdr] == obj2);
  UKFalse ([cons8 cdr] == obj2);
  UKTrue ([cons9 cdr] == obj2);
  
  [cons2 setCdr:obj1];
  UKTrue ([cons2 cdr] == obj1);

  [cons2 setCar:obj2];
  UKTrue ([cons2 car] == obj2);

  return nil;
}


-(id) testStuff
{
  // UKPass(); UKFail();
  //  UKNotNil (nil);
  //  UKTrue (1);
  //  UKStringsNotEqual (@"a", @"b");
  // UKPass();
  return nil;
}
@end
