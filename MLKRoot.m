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
#import "MLKInterpreter.h"
#import "MLKPackage.h"
#import "MLKRoot.h"
#import "MLKStream.h"
#import "MLKSymbol.h"
#import "MLKInteger.h"
#import "MLKSingleFloat.h"
#import "MLKDoubleFloat.h"
#import "runtime-compatibility.h"
#import "util.h"

#import <Foundation/NSArray.h>
#import <Foundation/NSException.h>
#import <Foundation/NSInvocation.h>
#import <Foundation/NSMethodSignature.h>
#import <Foundation/NSNull.h>
#import <Foundation/NSStream.h>
#import <Foundation/NSString.h>


static NSMethodSignature *signature;
static MLKPackage *sys;
static MLKPackage *cl;


static id truify (BOOL value)
{
  return (value ? (id) [cl intern:@"T"] : nil);
}

#define RETURN_VALUE(thing)                     \
  return [NSArray arrayWithObject:nullify(thing)];


@implementation MLKRoot
+(void) initialize
{
  signature = RETAIN ([self methodSignatureForSelector:@selector(car:)]);
  sys = [MLKPackage findPackage:@"TOILET-SYSTEM"];
  cl = [MLKPackage findPackage:@"COMMON-LISP"];
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

+(NSArray *) load:(NSArray *)args
{
  // FIXME
  BOOL success;
  NSString *fileName = denullify ([args objectAtIndex:0]);
  NSInputStream *input = [NSInputStream inputStreamWithFileAtPath:fileName];
  MLKStream *stream = AUTORELEASE ([[MLKStream alloc] initWithInputStream:input]);

  //NSLog (@"%d", [input hasBytesAvailable]);
  [input open];
  //NSLog (@"%d", [input hasBytesAvailable]);

  success = [MLKInterpreter load:stream verbose:YES print:YES];

  [input close];

  RETURN_VALUE (truify (success));
}

+(NSArray *) eq:(NSArray *)args
{
  RETURN_VALUE (truify ([args objectAtIndex:0] == [args objectAtIndex:1]));
}

+(NSArray *) symbolp:(NSArray *)args
{
  id arg0 = [args objectAtIndex:0];
  RETURN_VALUE (truify (arg0 == [NSNull null]
                        || [arg0 isKindOfClass:[MLKSymbol class]]));
}

+(NSArray *) listp:(NSArray *)args
{
  id arg0 = [args objectAtIndex:0];
  RETURN_VALUE (truify (arg0 == [NSNull null]
                        || [arg0 isKindOfClass:[MLKCons class]]));
}

+(NSArray *) consp:(NSArray *)args
{
  id arg0 = [args objectAtIndex:0];
  RETURN_VALUE (truify ([arg0 isKindOfClass:[MLKCons class]]));
}

+(NSArray *) atom:(NSArray *)args
{
  id arg0 = [args objectAtIndex:0];
  RETURN_VALUE (truify (![arg0 isKindOfClass:[MLKCons class]]));
}

+(NSArray *) null:(NSArray *)args
{
  RETURN_VALUE (truify ([args objectAtIndex:0] == [NSNull null]));
}

+(NSArray *) add:(NSArray *)args
{
  RETURN_VALUE ([[args objectAtIndex:0] add:[args objectAtIndex:1]]);
}

+(NSArray *) subtract:(NSArray *)args
{
  RETURN_VALUE ([[args objectAtIndex:0] subtract:[args objectAtIndex:1]]);
}

+(NSArray *) multiply:(NSArray *)args
{
  RETURN_VALUE ([[args objectAtIndex:0] multiplyWith:[args objectAtIndex:1]]);
}

+(NSArray *) divide:(NSArray *)args
{
  RETURN_VALUE ([[args objectAtIndex:0] divideBy:[args objectAtIndex:1]]);
}

+(NSArray *) list:(NSArray *)args
{
  RETURN_VALUE ([MLKCons listWithArray:args]);
}

+(NSArray *) macroexpand_1:(NSArray *)args
{
  id form = [args objectAtIndex:0];
  id env = [args count] > 1 ? [args objectAtIndex:1] : nil;
  MLKLexicalContext *context = env ? (id)env : (id)[MLKLexicalContext globalContext];

  if ([context symbolNamesMacro:[form car]])
    {
      id <MLKFuncallable> macrofun = [context macroForSymbol:[form car]];
      form = denullify ([[macrofun applyToArray:
                                     [NSArray arrayWithObjects:
                                                form, context, nil]]
                          objectAtIndex:0]);
    }

  RETURN_VALUE (form);
}
@end
