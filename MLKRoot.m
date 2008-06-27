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
#import "MLKRoot.h"
#import "MLKSymbol.h"
#import "runtime-compatibility.h"

#import <Foundation/NSArray.h>
#import <Foundation/NSException.h>
#import <Foundation/NSInvocation.h>
#import <Foundation/NSMethodSignature.h>
#import <Foundation/NSNull.h>
#import <Foundation/NSString.h>


static id nullify (id value)
{
  if (value)
    return value;
  else
    return [NSNull null];
}

static id denullify (id value)
{
  if (value == [NSNull null])
    return nil;
  else
    return value;
}


static NSMethodSignature *signature;
static MLKPackage *sys;


@implementation MLKRoot
+(void) initialize
{
  signature = RETAIN ([self methodSignatureForSelector:@selector(car:)]);
  sys = [MLKPackage findPackage:@"TOILET-SYSTEM"];
}

+(NSArray *) dispatch:(MLKSymbol *)name withArguments:(NSArray *)args
{
  NSInvocation *invocation;
  NSMutableString *methodName;
  NSArray *result;
  SEL selector;

  NS_DURING
    {
      if ([sys findSymbol:[name name]] != name)
        return nil;
    }
  NS_HANDLER
    {
      NS_VALUERETURN (nil, NSArray *);
    }
  NS_ENDHANDLER

  invocation = [NSInvocation invocationWithMethodSignature:signature];

  methodName = [NSMutableString stringWithString:[[name name] lowercaseString]];
  [methodName replaceOccurrencesOfString:@"-"
              withString:@"_"
              options:NSLiteralSearch
              range:NSMakeRange(0, [methodName length])];
  [methodName appendString:@":"];

  selector = NSSelectorFromString (methodName);
  
  if (!selector || ![self respondsToSelector:selector])
    return nil;

  [invocation setSelector:selector];
  [invocation setTarget:self];
  [invocation setArgument:&args atIndex:2];

  [invocation invoke];
  [invocation getReturnValue:&result];

  return result;
}

+(NSArray *) car:(NSArray *)args
{
  return [NSArray arrayWithObject:nullify([denullify([args objectAtIndex:0]) car])];
}

+(NSArray *) cdr:(NSArray *)args
{
  return [NSArray arrayWithObject:nullify([denullify([args objectAtIndex:0]) cdr])];
}

+(NSArray *) set_car:(NSArray *)args
{
  [[args objectAtIndex:0] setCar:denullify([args objectAtIndex:1])];
  return [NSArray arrayWithObject:[args objectAtIndex:1]];
}

+(NSArray *) set_cdr:(NSArray *)args
{
  [[args objectAtIndex:0] setCdr:denullify([args objectAtIndex:1])];
  return [NSArray arrayWithObject:[args objectAtIndex:1]];
}

+(NSArray *) cons:(NSArray *)args
{
  return [NSArray arrayWithObject:
                    [MLKCons cons:denullify([args objectAtIndex:0])
                             with:denullify([args objectAtIndex:1])]];
}
@end
