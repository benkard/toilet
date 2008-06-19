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
                           
#include <UnitKit/UnitKit.h>
#include <Foundation/Foundation.h>

#include "MLKCons.h"
#include "MLKDoubleFloat.h"
#include "MLKDynamicContext.h"
#include "MLKEnvironment.h"
#include "MLKLinkedList.h"
#include "MLKPackage.h"
#include "MLKRatio.h"
#include "MLKReader.h"
#include "MLKReadtable.h"
#include "MLKSingleFloat.h"
#include "MLKSymbol.h"

@interface MLKLowLevelTests : NSObject <UKTest>
@end


// static void MLKNSUncaughtExceptionHandler (NSException *exception)
// {
//   NSLog (@"Caught unhandled exception.\nName:%@\nReason:%@",
//          [exception name],
//          [exception reason]);
// }


@implementation MLKLowLevelTests
-(id) initForTest
{
  self = [super init];
  [MLKDynamicContext currentContext];

  //  NSSetUncaughtExceptionHandler (MLKNSUncaughtExceptionHandler);

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


-(id) testInitialReadtable
{
  MLKDynamicContext *ctx = [MLKDynamicContext currentContext];
  MLKReadtable *readtable = [ctx valueForBinding:
                                   [[MLKPackage findPackage:@"COMMON-LISP"]
                                     intern:@"*READTABLE*"]];
  UKTrue ([readtable characterHasCase:'a']);
  UKTrue ([readtable characterHasCase:'x']);
  UKTrue ([readtable characterHasCase:'F']);
  UKTrue ([readtable characterHasCase:228]);  // ä
  UKTrue ([readtable characterHasCase:196]);  // Ä
  UKFalse ([readtable characterHasCase:'=']);
  UKFalse ([readtable characterHasCase:'.']);
  UKFalse ([readtable characterHasCase:223]);  // ß

  return nil;
}


-(id) testTokens
{
  UKObjectKindOf ([MLKReader readFromString:@"a"], MLKSymbol);
  UKObjectKindOf ([MLKReader readFromString:@"MULK"], MLKSymbol);
  UKObjectKindOf ([MLKReader readFromString:@"+"], MLKSymbol);
  UKObjectKindOf ([MLKReader readFromString:@"1-"], MLKSymbol);
  UKObjectKindOf ([MLKReader readFromString:@"1+"], MLKSymbol);
  UKObjectKindOf ([MLKReader readFromString:@"0AA0A"], MLKSymbol);
  
  UKObjectKindOf ([MLKReader readFromString:@"0AA0A"], MLKSymbol);
  UKObjectKindOf ([MLKReader readFromString:@"0\\aA0A"], MLKSymbol);
  UKObjectKindOf ([MLKReader readFromString:@"\\0"], MLKSymbol);
  UKObjectKindOf ([MLKReader readFromString:@"|abc def (mulk!)|"], MLKSymbol);
  UKObjectKindOf ([MLKReader readFromString:@"0\\.3"], MLKSymbol);

  UKObjectKindOf ([MLKReader readFromString:@"134651234"], MLKInteger);
  UKObjectKindOf ([MLKReader readFromString:@"223555."], MLKInteger);
  UKObjectKindOf ([MLKReader readFromString:@"-134651234"], MLKInteger);
  UKObjectKindOf ([MLKReader readFromString:@"-223555."], MLKInteger);
  UKObjectKindOf ([MLKReader readFromString:@"+134651234"], MLKInteger);
  UKObjectKindOf ([MLKReader readFromString:@"+223555."], MLKInteger);
  UKObjectKindOf ([MLKReader readFromString:@"-1."], MLKInteger);
  UKObjectKindOf ([MLKReader readFromString:@"+2"], MLKInteger);
  UKObjectKindOf ([MLKReader readFromString:@"3."], MLKInteger);
  UKObjectKindOf ([MLKReader readFromString:@"3"], MLKInteger);
  
  UKObjectKindOf ([MLKReader readFromString:@"55/11"], MLKRatio);
  UKObjectKindOf ([MLKReader readFromString:@"-55/11"], MLKRatio);

  UKObjectKindOf ([MLKReader readFromString:@"1234.5678e99"], MLKSingleFloat);
  UKObjectKindOf ([MLKReader readFromString:@"-1234.5678e99"], MLKSingleFloat);
  UKObjectKindOf ([MLKReader readFromString:@"+1234.5678e99"], MLKSingleFloat);
  UKObjectKindOf ([MLKReader readFromString:@"1234.5678e-99"], MLKSingleFloat);
  UKObjectKindOf ([MLKReader readFromString:@"1234.5678e+99"], MLKSingleFloat);
  UKObjectKindOf ([MLKReader readFromString:@"-1234.5678e-99"], MLKSingleFloat);
  UKObjectKindOf ([MLKReader readFromString:@"1234.5678"], MLKSingleFloat);
  UKObjectKindOf ([MLKReader readFromString:@"-1234.5678"], MLKSingleFloat);
  UKObjectKindOf ([MLKReader readFromString:@".5678"], MLKSingleFloat);
  UKObjectKindOf ([MLKReader readFromString:@"-.5678"], MLKSingleFloat);
  UKObjectKindOf ([MLKReader readFromString:@"+.5678"], MLKSingleFloat);
  UKObjectKindOf ([MLKReader readFromString:@".5678e3"], MLKSingleFloat);
  UKObjectKindOf ([MLKReader readFromString:@"-.5678e3"], MLKSingleFloat);
  UKObjectKindOf ([MLKReader readFromString:@"+.5678e3"], MLKSingleFloat);

  UKStringsEqual ([[MLKReader readFromString:@"a"] name], @"A");
  UKStringsEqual ([[MLKReader readFromString:@"1+"] name], @"1+");
  UKStringsEqual ([[MLKReader readFromString:@"mulkmulk"] name], @"MULKMULK");
  UKStringsEqual ([[MLKReader readFromString:@"ABCDefghIJKL"] name], @"ABCDEFGHIJKL");
  UKStringsEqual ([[MLKReader readFromString:@"class-name"] name], @"CLASS-NAME");
  UKStringsEqual ([[MLKReader readFromString:@"\\class-\\name"] name], @"cLASS-nAME");
  UKStringsEqual ([[MLKReader readFromString:@"|Class Name|"] name], @"Class Name");
  UKStringsEqual ([[MLKReader readFromString:@"class\\ name"] name], @"CLASS NAME");
  UKStringsEqual ([[MLKReader readFromString:@"\\100"] name], @"100");
  
  UKStringsEqual ([[MLKReader readFromString:@"a b c d e"] name], @"A");

  return nil;
}


-(id) testParenReading
{
  UKObjectKindOf ([MLKReader readFromString:@"(1 2)"], MLKCons);
  UKObjectKindOf ([MLKReader readFromString:@"(1 . 2)"], MLKCons);
  UKObjectKindOf ([MLKReader readFromString:@"(a b)"], MLKCons);

  UKNil ([MLKReader readFromString:@"()"]);

  UKObjectKindOf ([[MLKReader readFromString:@"(1 . 2)"] car], MLKInteger);
  UKObjectKindOf ([[MLKReader readFromString:@"(1 . 2)"] cdr], MLKInteger);
  
  UKObjectKindOf ([[MLKReader readFromString:@"(a b)"] car], MLKSymbol);
  UKObjectKindOf ([[MLKReader readFromString:@"(a b)"] cdr], MLKCons);
  
  UKObjectKindOf ([[MLKReader readFromString:@"((a) b)"] car], MLKCons);

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
