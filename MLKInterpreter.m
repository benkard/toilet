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

#define _XOPEN_SOURCE 600
#define _GNU_SOURCE  // for RTLD_DEFAULT
#define _ISOC99_SOURCE

#import "MLKInterpretedClosure.h"
#import "MLKCons.h"
#import "MLKDynamicContext.h"
#import "MLKEnvironment.h"
#import "MLKForeignProcedure.h"
#import "MLKFuncallable.h"
#import "MLKInterpreter.h"
#import "MLKLexicalContext.h"
#import "MLKLexicalEnvironment.h"
#import "MLKLLVMCompiler.h"
#import "MLKPackage.h"
#import "MLKReader.h"
#import "MLKRoot.h"
#import "MLKSymbol.h"
#import "NSObject-MLKPrinting.h"
#import "globals.h"
#import "runtime-compatibility.h"
#import "special-symbols.h"
#import "util.h"

#import <Foundation/NSArray.h>
#import <Foundation/NSAutoreleasePool.h>
#import <Foundation/NSException.h>
#import <Foundation/NSNull.h>
#import <Foundation/NSString.h>
#import <Foundation/NSValue.h>

#include <stdio.h>

#ifdef _WIN32
  #include <windows.h>
#else
  #ifndef _BSD_SOURCE
    #define _BSD_SOURCE
  #endif
  #undef _POSIX_C_SOURCE  // needed at least on Mac OS X for RTLD_DEFAULT to be defined
  #include <dlfcn.h>    
#endif


#define RETURN_VALUE(thing)                     \
{ return [NSArray arrayWithObject:nullify(thing)]; }


static id PRIMARY (NSArray *array) __attribute__ ((pure));

static id
PRIMARY (NSArray *array)
{
  if ([array count] > 0)
    return [array objectAtIndex:0];
  else
    return nil;
}


@implementation MLKInterpreter
+(void) initialize
{
  ensure_symbols ();
}

+(id) compile:(id)object inContext:(MLKLexicalContext *)context
{
  return PRIMARY ([self eval:object
                        inLexicalContext:context
                        withEnvironment:[MLKLexicalEnvironment globalEnvironment]]);
}

+(NSArray*) eval:(id)program
            inLexicalContext:(MLKLexicalContext *)context
            withEnvironment:(MLKLexicalEnvironment *)lexenv
{
  id form = [MLKForm formWithObject:program
                          inContext:context
                        forCompiler:self];
  return [form interpretWithEnvironment:lexenv];
}

+(BOOL) load:(MLKStream *)stream verbose:(BOOL)verbose print:(BOOL)print
{
  id eofValue = [[NSObject alloc] init];
  int level = MLKIntWithInteger ([[MLKDynamicContext currentContext]
                                   valueForSymbol:[sys intern:@"*LOAD-LEVEL*"]]);
  int i;

  while (YES)
    {
      id result;
      id expansion;
      //NSLog (@"; LOAD: Reding a form.");
      id code;
      //NSLog (@"; LOAD: Reading finished.");
      NSString *formdesc;
      NSAutoreleasePool *pool;

      //NSLog (@"%@", MLKPrintToString(code));
      //NSLog (@"%@", stream);
      //NSLog (@"...");

      pool = [[NSAutoreleasePool alloc] init];

      code = [MLKReader readFromStream:stream
                        eofError:NO
                        eofValue:eofValue
                        recursive:NO
                        preserveWhitespace:NO];

      if (code == eofValue)
        break;

      if (MLKInstanceP(code)
          && [code isKindOfClass:[MLKCons class]] && [code cdr])
        formdesc = [NSString stringWithFormat:@"(%@ %@ ...)",
                               MLKPrintToString([code car]),
                               MLKPrintToString([[code cdr] car])];
      else
        formdesc = MLKPrintToString(code);

      //fprintf (stderr, "; COMPILE-MINIMALLY: %s\n", [formdesc UTF8String]);
      fprintf (stderr, "; ");
      for (i = 0; i < level; i++)
        fprintf (stderr, "| ");
      fprintf (stderr, "LOAD: %s\n", [formdesc UTF8String]);

      if (MLKLoadCompilesP)
        {
          expansion = code;
          result = [MLKDefaultCompiler eval:code];
        }
      else
        {
          expansion = code;
          result = [MLKInterpreter
                     eval:expansion
                     inLexicalContext:[MLKLexicalContext globalContext]
                     withEnvironment:[MLKLexicalEnvironment globalEnvironment]];
        }

      LRELEASE (pool);

      if (print)
        {
          //FIXME
          //NSLog (@"; LOAD: Fnord.  Primary value: %@",
          //       MLKPrintToString([result objectAtIndex:0]));
        }
    }

  return YES;
}
@end


@implementation MLKForm (MLKInterpretation)
-(NSArray *) interpret
{
  return [self interpretWithEnvironment:[MLKLexicalEnvironment globalEnvironment]];
}


-(NSArray *) interpretWithEnvironment:(MLKLexicalEnvironment *)env
{
  NSArray *values;

#if 0
  BOOL trace = NO;
  //if ([dynamicContext valueForSymbol:V_INITP])
  //  trace = YES;
  
  //if (trace)
  NSLog (@"; EVAL END: %@", MLKPrintToString(_form));
  values = [self reallyInterpretWithEnvironment:env];
  //if (trace)
  NSLog (@"; EVAL: %@", MLKPrintToString(_form));
#elif 0
  NS_DURING
    {
      values = [self reallyInterpretWithEnvironment:env];
    }
  NS_HANDLER
    {
      NSLog (@"; BROKEN EVAL: %@", MLKPrintToString(_form));
      [localException raise];
    }
  NS_ENDHANDLER;
#else
  values = [self reallyInterpretWithEnvironment:env];  
#endif

  return values;
}


-(NSArray *) reallyInterpretWithEnvironment:(MLKLexicalEnvironment *)env
{
  NSLog (@"WARNING: Unrecognised form type: %@", self);
  return [NSArray array];
}
@end


@implementation MLKSelfEvaluatingForm (MLKInterpretation)
-(NSArray *) reallyInterpretWithEnvironment:(MLKLexicalEnvironment *)env
{
  RETURN_VALUE (_form);
}
@end


@implementation MLKSymbolForm (MLKInterpretation)
-(NSArray *) reallyInterpretWithEnvironment:(MLKLexicalEnvironment *)env
{
  if ([_context variableIsLexical:_form])
    {
//      NSLog (@"Lexical?");
      RETURN_VALUE ([env valueForSymbol:_form]);
    }
  else
    {
//      NSLog (@"Special?");
      RETURN_VALUE ([[MLKDynamicContext currentContext] valueForSymbol:_form]);
    }
}
@end


@implementation MLKCatchForm (MLKInterpretation)
-(NSArray *) reallyInterpretWithEnvironment:(MLKLexicalEnvironment *)env
{
  id catchTag;
  NSArray *values;
  MLKDynamicContext *newctx;

  catchTag = PRIMARY ([_tagForm interpretWithEnvironment:env]);

  NS_DURING
    {
      newctx = [[MLKDynamicContext alloc] initWithParent:[MLKDynamicContext currentContext]
                                                variables:nil
                                                 handlers:nil
                                                 restarts:nil
                                                catchTags:[NSSet setWithObject:catchTag]
                                 activeHandlerEnvironment:nil];
      [newctx pushContext];

      values = [self interpretBodyWithEnvironment:env];

      NS_VALUERETURN (values, NSArray *);
    }
  NS_HANDLER
    {
      [MLKDynamicContext popContext];
      LRELEASE (newctx);
        
      if ([[localException name] isEqualToString:@"MLKThrow"])
        {
          id thrownTag = [[localException userInfo]
                                       objectForKey:@"THROWN TAG"];
          
          if (thrownTag == catchTag)
            return [[localException userInfo]
                                 objectForKey:@"THROWN OBJECTS"];
          else
            [localException raise];
        }
      else
        [localException raise];
    }
  NS_ENDHANDLER;

  [MLKDynamicContext popContext];
  LRELEASE (newctx);

  return nil;      
}
@end


@implementation MLKForeignLambdaForm (MLKInterpretation)
-(NSArray *) reallyInterpretWithEnvironment:(MLKLexicalEnvironment *)env
{
  // FIXME: Support library designators.
  
  int (*function)();

#ifdef _WIN32
  // FIXME
  //EnumProcessModules (...);
  //GetProcAddress (..., [_foreignName UTF8String]);
#else
  function = dlsym (RTLD_DEFAULT, [_foreignName UTF8String]);
#endif

  NSMutableArray *argtypes = [NSMutableArray array];
  int i;
  for (i = 0; i < _argc; i++)
    [argtypes addObject:[NSNumber numberWithInt:_argumentTypes[i]]];

  RETURN_VALUE (LAUTORELEASE ([[MLKForeignProcedure alloc]
                               initWithCode:function
                              argumentTypes:argtypes
                                 returnType:_returnType]));  
}
@end


@implementation MLKLambdaFunctionForm (MLKInterpretation)
-(NSArray *) reallyInterpretWithEnvironment:(MLKLexicalEnvironment *)env
{
  return [_lambdaForm interpretWithEnvironment:env];
}
@end


@implementation MLKSimpleFunctionForm (MLKInterpretation)
-(NSArray *) reallyInterpretWithEnvironment:(MLKLexicalEnvironment *)env
{
  RETURN_VALUE ([env functionForSymbol:_functionName]);
}
@end


@implementation MLKIfForm (MLKInterpretation)
-(NSArray *) reallyInterpretWithEnvironment:(MLKLexicalEnvironment *)env
{
  id cndval = denullify(PRIMARY([_conditionForm interpretWithEnvironment:env]));
  if (cndval)
    return [_consequentForm interpretWithEnvironment:env];
  else
    return [_alternativeForm interpretWithEnvironment:env];
}
@end


@implementation MLKInPackageForm (MLKInterpretation)
-(NSArray *) reallyInterpretWithEnvironment:(MLKLexicalEnvironment *)env
{
  id package = [MLKPackage findPackage:stringify(_packageDesignator)];

  [[MLKDynamicContext currentContext]
   setValue:package
   forSymbol:[[MLKPackage findPackage:@"COMMON-LISP"]
                            intern:@"*PACKAGE*"]];

  RETURN_VALUE (package);
}
@end


@implementation MLKSimpleLambdaForm (MLKInterpretation)
-(NSArray *) reallyInterpretWithEnvironment:(MLKLexicalEnvironment *)env
{
  id closure = LAUTORELEASE ([[MLKInterpretedClosure alloc]
                              initWithForm:self
                              environment:env]);
  RETURN_VALUE (closure);  
}
@end


@implementation MLKSimpleFletForm (MLKInterpretation)
-(NSArray *) reallyInterpretWithEnvironment:(MLKLexicalEnvironment *)env
{
  int i;
  MLKLexicalEnvironment *newenv = [MLKLexicalEnvironment environmentWithParent:env
                                                                     variables:nil
                                                                     functions:nil];

  for (i = 0; i < [_functionBindingForms count]; i++)
    {
      [[_functionBindingForms objectAtIndex:i] interpretWithEnvironment:newenv];
    }

  return [self interpretBodyWithEnvironment:newenv];
}
@end


@implementation MLKSimpleFunctionBindingForm (MLKInterpretation)
-(NSArray *) reallyInterpretWithEnvironment:(MLKLexicalEnvironment *)env
{
  id value = [_compiler compile:[MLKCons cons:_LAMBDA with:_tail]
                      inContext:_context];
  [env addFunction:value forSymbol:_name];
  return nil;
}
@end


@implementation MLKLetForm (MLKInterpretation)
-(NSArray *) reallyInterpretWithEnvironment:(MLKLexicalEnvironment *)env
{
  int i;
  NSArray *values;
  MLKLexicalEnvironment *newenv;
  MLKDynamicContext *dynctx;

  newenv = [MLKLexicalEnvironment environmentWithParent:env
                                              variables:nil
                                              functions:nil];
  dynctx = [[MLKDynamicContext alloc] initWithParent:[MLKDynamicContext currentContext]
                                           variables:nil
                                            handlers:nil
                                            restarts:nil
                                           catchTags:nil
                            activeHandlerEnvironment:nil];
  
  for (i = 0; i < [_variableBindingForms count]; i++)
    {
      id variable = [[_variableBindingForms objectAtIndex:i] name];
      id value = PRIMARY([[_variableBindingForms objectAtIndex:i] 
                           interpretWithEnvironment:env]);
      if ([_bodyContext variableIsLexical:variable])
        {
          [newenv addValue:value forSymbol:variable];
        }
      else
        {
          [dynctx addValue:value forSymbol:variable];
        }
    }

  [dynctx pushContext];

  NS_DURING
    {
      values = [self interpretBodyWithEnvironment:newenv];
    }
  NS_HANDLER
    {
      [MLKDynamicContext popContext];
      LRELEASE (dynctx);
      [localException raise];
    }
  NS_ENDHANDLER;

  [MLKDynamicContext popContext];
  LRELEASE (dynctx);

  return values;
}
@end


@implementation MLKVariableBindingForm (MLKInterpretation)
-(NSArray *) reallyInterpretWithEnvironment:(MLKLexicalEnvironment *)env
{
  return [_valueForm interpretWithEnvironment:env];
}
@end


@implementation MLKSimpleLoopForm (MLKInterpretation)
-(NSArray *) reallyInterpretWithEnvironment:(MLKLexicalEnvironment *)env
{
  while (YES)
    {
      [self interpretBodyWithEnvironment:env];
    }

  RETURN_VALUE (nil);  // never reached
}
@end


@implementation MLKMultipleValueCallForm (MLKInterpretation)
-(NSArray *) reallyInterpretWithEnvironment:(MLKLexicalEnvironment *)env
{
  NSMutableArray *results = [NSMutableArray array];
  int i;
  id <MLKFuncallable> function = PRIMARY ([_functionForm interpretWithEnvironment:env]);

  for (i = 0; i < [_bodyForms count]; i++)
    {
      NSArray *values = [[_bodyForms objectAtIndex:i] interpretWithEnvironment:env];
      [results addObjectsFromArray:values];
    }
  
  return [function applyToArray:results];
}
@end


@implementation MLKProgNForm (MLKInterpretation)
-(NSArray *) reallyInterpretWithEnvironment:(MLKLexicalEnvironment *)env
{
  return [self interpretBodyWithEnvironment:env];
}
@end


@implementation MLKBodyForm (MLKInterpretation)
-(NSArray *) interpretBodyWithEnvironment:(MLKLexicalEnvironment *)env
{
  int i;
  NSArray *values = [NSArray array];
 
  for (i = 0; i < [_bodyForms count]; i++)
    {
      values = [[_bodyForms objectAtIndex:i] interpretWithEnvironment:env];
    }
  
  return values;  
}
@end


@implementation MLKProgVForm (MLKInterpretation)
-(NSArray *) reallyInterpretWithEnvironment:(MLKLexicalEnvironment *)env
{
  id variables = PRIMARY([_variableListForm interpretWithEnvironment:env]);
  id values = PRIMARY([_valueListForm interpretWithEnvironment:env]);
  MLKDynamicContext *dynctx;
  id result;

  dynctx = [[MLKDynamicContext alloc]
              initWithParent:[MLKDynamicContext currentContext]
                   variables:nil
                    handlers:nil
                    restarts:nil
                   catchTags:nil
    activeHandlerEnvironment:nil];

  for (; variables; (variables = [variables cdr], values = [values cdr]))
    {
      id var = [variables car];
      id value = [values car];

      [dynctx addValue:value forSymbol:var];
    }

  [dynctx pushContext];

  NS_DURING
    {
      result = [self interpretBodyWithEnvironment:env];
    }
  NS_HANDLER
    {
      [MLKDynamicContext popContext];
      LRELEASE (dynctx);
      [localException raise];
    }
  NS_ENDHANDLER;

  [MLKDynamicContext popContext];
  LRELEASE (dynctx);

  return result;     
}
@end


@implementation MLKQuoteForm (MLKInterpretation)
-(NSArray *) reallyInterpretWithEnvironment:(MLKLexicalEnvironment *)env
{
  RETURN_VALUE (_quotedData);
}
@end


@implementation MLKThrowForm (MLKInterpretation)
-(NSArray *) reallyInterpretWithEnvironment:(MLKLexicalEnvironment *)env
{
  id catchTag;
  NSArray *values;
  NSDictionary *userInfo;

  catchTag = PRIMARY([_tagForm interpretWithEnvironment:env]);
  values = [_valueForm interpretWithEnvironment:env];

  userInfo = [NSDictionary dictionaryWithObjectsAndKeys:
    catchTag, @"THROWN TAG",
    values, @"THROWN OBJECTS", nil];

  if ([[MLKDynamicContext currentContext] catchTagIsEstablished:denullify (catchTag)])
    [[NSException exceptionWithName:@"MLKThrow"
                             reason:[NSString stringWithFormat:
                                                       @"THROW: tag %@, values %@.",
                                                       MLKPrintToString(catchTag),
                                                       MLKPrintToString(values)]
                           userInfo:userInfo] raise];
  else
    // FIXME: This should really be a condition rather than
    // an exception.  See CLHS THROW.
    [[NSException exceptionWithName:@"MLKControlError"
                             reason:[NSString stringWithFormat:
                                                @"THROW without a corresponding CATCH: tag %@, values %@.",
                                                MLKPrintToString(catchTag),
                                                MLKPrintToString(values)]
                           userInfo:userInfo] raise];

  return nil;
}
@end


@implementation MLKUnwindProtectForm (MLKInterpretation)
-(NSArray *) reallyInterpretWithEnvironment:(MLKLexicalEnvironment *)env
{
  NSArray *results;

  NS_DURING
    {
      results = [_protectedForm interpretWithEnvironment:env];
    }
  NS_HANDLER
    {
      [self interpretBodyWithEnvironment:env];
      [localException raise];
    }
  NS_ENDHANDLER;

  [self interpretBodyWithEnvironment:env];

  return results;      
}
@end


@implementation MLKFunctionCallForm (MLKInterpretation)
-(NSArray *) reallyInterpretWithEnvironment:(MLKLexicalEnvironment *)env
{
  int i;
  NSMutableArray *args = [NSMutableArray array];

  for (i = 0; i < [_argumentForms count]; i++)
    {
      id result = PRIMARY([[_argumentForms objectAtIndex:i]
                            interpretWithEnvironment:env]);
      [args addObject:result];
    }

  if (![_context symbolNamesFunction:_head])
    {
      [NSException raise:@"MLKNoSuchOperatorException"
                   format:@"%@ does not name a known operator.",
                          MLKPrintToString(_head)];
      return nil;
    }
  else
    {
      id <MLKFuncallable> function = [env functionForSymbol:_head];
      return [function applyToArray:args];
    }
}
@end


@implementation MLKSetQForm (MLKInterpretation)
-(NSArray *) reallyInterpretWithEnvironment:(MLKLexicalEnvironment *)env
{
  int i;
  id value = nil;
  MLKDynamicContext *dynamicContext = [MLKDynamicContext currentContext];

  for (i = 0; i < [_variables count]; i++)
    {
      id symbol = denullify([_variables objectAtIndex:i]);
      value = PRIMARY([[_valueForms objectAtIndex:i] interpretWithEnvironment:env]);

      if ([_context variableIsLexical:symbol])
        [env setValue:value forSymbol:symbol];
      else if ([dynamicContext bindingForSymbol:symbol])
        [dynamicContext setValue:value forSymbol:symbol];
      else
        // FIXME: Maybe print a warning.
        [[MLKDynamicContext globalContext] addValue:value
                                          forSymbol:symbol];
    }

  RETURN_VALUE (value);
}
@end


@implementation MLKFSetQForm (MLKInterpretation)
-(NSArray *) reallyInterpretWithEnvironment:(MLKLexicalEnvironment *)env
{
  int i;
  id value = nil;

  for (i = 0; i < [_functionNames count]; i++)
    {
      id symbol = denullify([_functionNames objectAtIndex:i]);
      value = PRIMARY([[_valueForms objectAtIndex:i] interpretWithEnvironment:env]);

      if ([_context symbolNamesFunction:symbol])
        {
          [env setFunction:value forSymbol:symbol];
        }
      else
        {
          // FIXME: Maybe print a warning.
          [[MLKLexicalContext globalContext] addFunction:symbol];
          [[MLKLexicalEnvironment globalEnvironment] addFunction:value
                                                       forSymbol:symbol];
        }
    }

  RETURN_VALUE (value);
}
@end
