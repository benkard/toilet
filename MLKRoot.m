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

#import "MLKBinding.h"
#import "MLKCharacter.h"
#import "MLKCons.h"
#import "MLKDynamicContext.h"
#import "MLKInterpretedClosure.h"
#import "MLKInterpreter.h"
#import "MLKLLVMCompiler.h"
#import "MLKNumber.h"
#import "MLKPackage.h"
#import "MLKRoot.h"
#import "MLKStream.h"
#import "MLKSymbol.h"
#import "MLKInteger.h"
#import "MLKSingleFloat.h"
#import "MLKDoubleFloat.h"
#import "NSObject-MLKPrinting.h"
#import "globals.h"
#import "runtime-compatibility.h"
#import "util.h"

#import <Foundation/NSArray.h>
#import <Foundation/NSException.h>
#import <Foundation/NSInvocation.h>
#import <Foundation/NSMethodSignature.h>
#import <Foundation/NSNull.h>
#import <Foundation/NSStream.h>
#import <Foundation/NSString.h>

#include <alloca.h>


static NSMethodSignature *signature;
static MLKPackage *sys;
static MLKPackage *cl;


static id truify (BOOL value)
{
  return (value ? (id) [cl intern:@"T"] : nil);
}

#define RETURN_VALUE(thing)                     \
  { return [NSArray arrayWithObject:nullify(thing)]; }


@implementation MLKRoot
+(void) initialize
{
  signature = LRETAIN ([self methodSignatureForSelector:@selector(car:)]);
  sys = [MLKPackage findPackage:@"TOILET-SYSTEM"];
  cl = [MLKPackage findPackage:@"COMMON-LISP"];
}

+(NSArray *) dispatch:(MLKSymbol *)name withArguments:(NSArray *)args
{
  NSInvocation *invocation;
  NSMutableString *methodName;
  NSArray *result;
  SEL selector;
  BOOL nothing_found;

  nothing_found = NO;

  NS_DURING
    {
      if ([sys findSymbol:[name name]] != name)
        NS_VALUERETURN (nil, NSArray *);
    }
  NS_HANDLER
    {
      nothing_found = YES;
    }
  NS_ENDHANDLER;

  if (nothing_found)
    return nil;

  invocation = [NSInvocation invocationWithMethodSignature:signature];

  methodName = [NSMutableString stringWithString:[[name name] lowercaseString]];
  [methodName replaceOccurrencesOfString:@"-"
              withString:@"_"
              options:NSLiteralSearch
              range:NSMakeRange(0, [methodName length])];
  [methodName replaceOccurrencesOfString:@"%"
              withString:@""
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

+(NSArray *) rplaca:(NSArray *)args
{
  MLKCons *cons = [args objectAtIndex:0];
  [cons setCar:denullify([args objectAtIndex:1])];
  RETURN_VALUE (cons);
}

+(NSArray *) rplacd:(NSArray *)args
{
  MLKCons *cons = [args objectAtIndex:0];
  [cons setCdr:denullify([args objectAtIndex:1])];
  RETURN_VALUE (cons);  
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
  int l, i;
  NSString *fileName = denullify ([args objectAtIndex:0]);
  NSInputStream *input = [NSInputStream inputStreamWithFileAtPath:fileName];
  MLKStream *stream = LAUTORELEASE ([[MLKStream alloc] initWithInputStream:input]);
  MLKDynamicContext *oldContext = [MLKDynamicContext currentContext];
  int level = MLKIntWithInteger ([oldContext
                                   valueForSymbol:[sys intern:@"*LOAD-LEVEL*"]]);
  MLKDynamicContext *ctx;

  l = [fileName length];
  fprintf (stderr, ";\n;  ");
  for (i = 0; i < 68 - 2*level; i++)
    fprintf (stderr, "_");

  fprintf (stderr, "\n; /");
  for (i = 0; i < 30 - l/2 - level; i++)
    fprintf (stderr, "-");
  fprintf (stderr, " LOAD: %s ", [fileName UTF8String]);
  for (i = 0; i < 30 - (l+1)/2 - level; i++)
    fprintf (stderr, "-");
  fprintf (stderr, "\n; |\n");

  //NSLog (@"%d", [input hasBytesAvailable]);
  [input open];
  //NSLog (@"%d", [input hasBytesAvailable]);

  ctx = [[MLKDynamicContext alloc]
          initWithParent:oldContext
          variables:nil
          handlers:nil
          restarts:nil
          catchTags:nil
          activeHandlerEnvironment:nil];
  [ctx addValue:MLKIntegerWithInt(level + 1)
       forSymbol:[sys intern:@"*LOAD-LEVEL*"]];
  [ctx pushContext];

  NS_DURING
    {
      success = [MLKInterpreter load:stream verbose:YES print:YES];
    }
  NS_HANDLER
    {
      [MLKDynamicContext popContext];
      LRELEASE (ctx);
      [input close];
      [localException raise];
    }
  NS_ENDHANDLER;

  [MLKDynamicContext popContext];
  LRELEASE (ctx);
  [input close];

  fprintf (stderr, "; \\");
  for (i = 0; i < 68 - 2*level; i++)
    fprintf (stderr, "_");
  fprintf (stderr, "\n; \n");

  RETURN_VALUE (truify (success));
}

+(NSArray *) eq:(NSArray *)args
{
  RETURN_VALUE (truify ([args objectAtIndex:0] == [args objectAtIndex:1]));
}

+(NSArray *) fixnum_eq:(NSArray *)args
{
#ifdef NO_FIXNUMS
  RETURN_VALUE (truify ([[args objectAtIndex:0]
                          isEqual:[args objectAtIndex:1]]));
#else
  RETURN_VALUE (truify (denullify([args objectAtIndex:0])
                        == denullify([args objectAtIndex:1])));
#endif
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

+(NSArray *) fixnump:(NSArray *)args
{
  id arg0 = denullify ([args objectAtIndex:0]);
  RETURN_VALUE (truify (MLKFixnumP (arg0)));
}

+(NSArray *) add:(NSArray *)args
{
  RETURN_VALUE ([((MLKNumber*)[args objectAtIndex:0]) add:[args objectAtIndex:1]]);
}

+(NSArray *) subtract:(NSArray *)args
{
  RETURN_VALUE ([((MLKNumber*)[args objectAtIndex:0]) subtract:[args objectAtIndex:1]]);
}

+(NSArray *) multiply:(NSArray *)args
{
  RETURN_VALUE ([((MLKNumber*)[args objectAtIndex:0]) multiplyWith:[args objectAtIndex:1]]);
}

+(NSArray *) divide:(NSArray *)args
{
  RETURN_VALUE ([((MLKNumber*)[args objectAtIndex:0]) divideBy:[args objectAtIndex:1]]);
}

+(NSArray *) add_fixnums:(NSArray *)args
{
  RETURN_VALUE (MLKAddFixnums (denullify([args objectAtIndex:0]),
                               denullify([args objectAtIndex:1])));
}

+(NSArray *) subtract_fixnums:(NSArray *)args
{
  RETURN_VALUE (MLKSubtractFixnums (denullify([args objectAtIndex:0]),
                                    denullify([args objectAtIndex:1])));
}

+(NSArray *) multiply_fixnums:(NSArray *)args
{
  RETURN_VALUE (MLKMultiplyFixnums (denullify([args objectAtIndex:0]),
                                    denullify([args objectAtIndex:1])));
}

+(NSArray *) idivide_fixnums:(NSArray *)args
{
  RETURN_VALUE (MLKIDivideFixnums (denullify([args objectAtIndex:0]),
                                   denullify([args objectAtIndex:1])));
}

+(NSArray *) list:(NSArray *)args
{
  RETURN_VALUE ([MLKCons listWithArray:args]);
}

+(NSArray *) macroexpand_1:(NSArray *)args
{
  id form = [args objectAtIndex:0];
  id env = [args count] > 1 ? denullify([args objectAtIndex:1]) : nil;
  MLKLexicalContext *context = env ? (id)env : (id)[MLKLexicalContext globalContext];
  id <MLKFuncallable> macrofun = nil;

  if ([form isKindOfClass:[MLKCons class]]
      && (![form car] || [[form car] isKindOfClass:[MLKSymbol class]])
      && [context symbolNamesMacro:[form car]])
    {
      macrofun = [context macroForSymbol:[form car]];
    }
  else if ([form isKindOfClass:[MLKSymbol class]]
           && [context symbolNamesSymbolMacro:form])
    {
      macrofun = [context symbolMacroForSymbol:[form car]];
    }

  if (macrofun)
    {
      form = denullify ([[macrofun applyToArray:
                                     [NSArray arrayWithObjects:
                                                form, context, nil]]
                          objectAtIndex:0]);
    }

  RETURN_VALUE (form);
}

+(NSArray *) shadow:(NSArray *)args
{
  id symbols = denullify ([args objectAtIndex:0]);
  id package = denullify (([args count] > 1
                           ? [args objectAtIndex:1]
                           : [[MLKDynamicContext currentContext]
                               valueForSymbol:
                                 [[MLKPackage findPackage:@"COMMON-LISP"]
                                   intern:@"*PACKAGE*"]]));

  if (![symbols isKindOfClass:[MLKCons class]])
    symbols = [MLKCons cons:symbols with:nil];

  do
    {
      [package shadow:stringify([symbols car])];
    }
  while ((symbols = [symbols cdr]));

  RETURN_VALUE ([cl intern:@"T"]);
}

+(NSArray *) export:(NSArray *)args
{
  id symbols = denullify ([args objectAtIndex:0]);
  id package = denullify (([args count] > 1
                           ? [args objectAtIndex:1]
                           : [[MLKDynamicContext currentContext]
                               valueForSymbol:
                                 [[MLKPackage findPackage:@"COMMON-LISP"]
                                   intern:@"*PACKAGE*"]]));

  if (![symbols isKindOfClass:[MLKCons class]])
    symbols = [MLKCons cons:symbols with:nil];

  do
    {
      [package export:[symbols car]];
    }
  while ((symbols = [symbols cdr]));

  RETURN_VALUE ([cl intern:@"T"]);
}

+(NSArray *) unexport:(NSArray *)args
{
  id symbols = denullify ([args objectAtIndex:0]);
  id package = denullify (([args count] > 1
                           ? [args objectAtIndex:1]
                           : [[MLKDynamicContext currentContext]
                               valueForSymbol:
                                 [[MLKPackage findPackage:@"COMMON-LISP"]
                                   intern:@"*PACKAGE*"]]));

  if (![symbols isKindOfClass:[MLKCons class]])
    symbols = [MLKCons cons:symbols with:nil];

  do
    {
      [package unexport:[symbols car]];
    }
  while ((symbols = [symbols cdr]));

  RETURN_VALUE ([cl intern:@"T"]);
}

+(NSArray *) find_package:(NSArray *)args
{
  NSString *name = stringify (denullify ([args objectAtIndex:0]));
  MLKPackage *package = [MLKPackage findPackage:name];

  if (package)
    {
      RETURN_VALUE (package);
    }
  else
    {
      [NSException raise:@"MLKNoSuchPackageError"
                   format:@"The package %@ does not exist",
                          MLKPrintToString(name)];
      return nil;
    }
}

+(NSArray *) string:(NSArray *)args
{
  RETURN_VALUE (stringify (denullify ([args objectAtIndex:0])));
}

+(NSArray *) gensym:(NSArray *)args
{
  NSString *prefix;
  NSString *suffix;
  MLKBinding *gensymCounter = [[MLKDynamicContext currentContext]
                                bindingForSymbol:
                                  [[MLKPackage findPackage:@"COMMON-LISP"]
                                    intern:@"*GENSYM-COUNTER*"]];

  if ([args count] > 0)
    {
      id x = [args objectAtIndex:0];
      if ([x isKindOfClass:[NSString class]])
        {
          prefix = x;
          suffix = MLKPrintToString([gensymCounter value]);
          [gensymCounter
            setValue:[(MLKInteger*)[gensymCounter value]
                       add:[MLKInteger integerWithInt:1]]];
        }
      else if ([x isKindOfClass:[MLKInteger class]])
        {
          // x must be an integer.
          prefix = @"G";
          suffix = MLKPrintToString(x);
        }
      else
        {
          [NSException raise:@"MLKTypeError"
                       format:@"%@ is not of type (OR INTEGER STRING).", x];
          return nil;
        }
    }
  else
    {
      prefix = @"G";
      suffix = MLKPrintToString([gensymCounter value]);
      [gensymCounter
        setValue:[(MLKInteger*)[gensymCounter value]
                   add:[MLKInteger integerWithInt:1]]];
    }

  RETURN_VALUE (([MLKSymbol symbolWithName:[NSString stringWithFormat:@"%@%@",
                                                                      prefix,
                                                                      suffix]
                            package:nil]));
}

+(NSArray *) make_symbol:(NSArray *)args
{
  NSString *name = [args objectAtIndex:0];

  RETURN_VALUE ([MLKSymbol symbolWithName:name package:nil]);
}

+(NSArray *) intern:(NSArray *)args
{
  NSString *name = [args objectAtIndex:0];
  id package = denullify (([args count] > 1
                           ? [args objectAtIndex:1]
                           : [[MLKDynamicContext currentContext]
                               valueForSymbol:
                                 [[MLKPackage findPackage:@"COMMON-LISP"]
                                   intern:@"*PACKAGE*"]]));
  MLKSymbol *symbol = [package intern:name];

  RETURN_VALUE (symbol);
}

+(NSArray *) import:(NSArray *)args
{
  MLKSymbol *symbol = [args objectAtIndex:0];
  id package = denullify (([args count] > 1
                           ? [args objectAtIndex:1]
                           : [[MLKDynamicContext currentContext]
                               valueForSymbol:
                                 [[MLKPackage findPackage:@"COMMON-LISP"]
                                   intern:@"*PACKAGE*"]]));

  [package import:symbol];

  RETURN_VALUE ([cl intern:@"T"]);
}

+(NSArray *) objc_class_of:(NSArray *)args
{
  RETURN_VALUE ([[args objectAtIndex:0] class]);
}

+(NSArray *) objc_subclassp:(NSArray *)args
{
  RETURN_VALUE (truify ([[args objectAtIndex:0] isSubclassOfClass:
                                                  [args objectAtIndex:1]]));
}

+(NSArray *) find_objc_class:(NSArray *)args
{
  RETURN_VALUE (NSClassFromString ([args objectAtIndex:0]));
}

+(NSArray *) ns_log:(NSArray *)args
{
  NSString *description = MLKPrintToString([args objectAtIndex:0]);
  NSLog (@"%@", description);
  RETURN_VALUE ([args objectAtIndex:0]);
}

+(NSArray *) symbol_name:(NSArray *)args
{
  MLKSymbol *symbol = denullify ([args objectAtIndex:0]);
  RETURN_VALUE (symbol ? (id)[symbol name] : (id)@"NIL");
}

+(NSArray *) primitive_type_of:(NSArray *)args
{
  id object = denullify ([args objectAtIndex:0]);

  if (!object)
    { RETURN_VALUE ([cl intern:@"NULL"]); }
  else if (MLKFixnumP (object))
    { RETURN_VALUE ([cl intern:@"FIXNUM"]); }
  else if ([object isKindOfClass:[MLKSymbol class]])
    { RETURN_VALUE ([cl intern:@"SYMBOL"]); }
  else if ([object isKindOfClass:[MLKCons class]])
    { RETURN_VALUE ([cl intern:@"CONS"]); }
  else if ([object isKindOfClass:[MLKDoubleFloat class]])
    { RETURN_VALUE ([cl intern:@"DOUBLE-FLOAT"]); }
  else if ([object isKindOfClass:[MLKSingleFloat class]])
    { RETURN_VALUE ([cl intern:@"SINGLE-FLOAT"]); }
  else if ([object isKindOfClass:[MLKInteger class]])
    { RETURN_VALUE ([cl intern:@"INTEGER"]); }
  else if ([object isKindOfClass:[MLKCharacter class]])
    //FIXME: STANDARD-CHAR
    { RETURN_VALUE ([cl intern:@"BASE-CHAR"]); }
  else if ([object isKindOfClass:[MLKInterpretedClosure class]])
    { RETURN_VALUE ([cl intern:@"FUNCTION"]); }
  else if ([object isKindOfClass:[MLKLexicalContext class]])
    { RETURN_VALUE ([sys intern:@"LEXICAL-CONTEXT"]); }
  else if ([object isKindOfClass:[MLKBinding class]])
    { RETURN_VALUE ([sys intern:@"BINDING"]); }
  else if ([object isKindOfClass:[MLKPackage class]])
    { RETURN_VALUE ([cl intern:@"PACKAGE"]); }
  else if ([object isKindOfClass:[MLKStream class]])
    { RETURN_VALUE ([cl intern:@"STREAM"]); }
  else if ([object isKindOfClass:[NSException class]])
    { RETURN_VALUE ([sys intern:@"EXCEPTION"]); }
  else if ([object isKindOfClass:[NSArray class]])
    { RETURN_VALUE ([cl intern:@"ARRAY"]); }
  else
    { RETURN_VALUE ([cl intern:@"T"]); }
}

+(NSArray *) send_by_name:(NSArray *)args
{
  NSString *methodName = denullify ([args objectAtIndex:1]);
  id object = denullify ([args objectAtIndex:0]);
  NSInvocation *invocation;
  SEL selector;
  NSMethodSignature *signature;
  int i;
  MLKForeignType returnType;

  if (MLKFixnumP (object))
    object = [MLKInteger integerWithFixnum:object];

  selector = NSSelectorFromString (methodName);
  if (!selector)
    {
      [NSException raise:@"MLKNoSuchSelectorError"
                   format:@"Could not find a selector named %@", methodName];
    }

  signature = [object methodSignatureForSelector:selector];
  if (!signature)
    {
      [NSException raise:@"MLKDoesNotUnderstandError"
                   format:@"%@ does not respond to selector %@", object, methodName];
    }

  invocation = [NSInvocation invocationWithMethodSignature:signature];

  [invocation setSelector:selector];
  [invocation setTarget:object];

  for (i = 2; i < [args count]; i++)
    {
      id argument = denullify ([args objectAtIndex:i]);
      const char *objctype = [signature getArgumentTypeAtIndex:i];
      MLKForeignType type = MLKForeignTypeWithObjectiveCType (objctype);
      ffi_type *ffi_argtype = MLKFFITypeWithForeignType (type);
      void *argbuf = alloca (ffi_argtype->size);

      if (type == MLKT_INVALID)
        [NSException raise:@"MLKInvalidArgumentError"
                     format:@"Don't know how to coerce %@ into type \"%s\".",
                     argument, objctype];

      MLKSetForeignValueWithLispValue (argbuf, argument, type);
      [invocation setArgument:argbuf atIndex:i];
    }

  [invocation invoke];


  returnType = MLKForeignTypeWithObjectiveCType ([signature methodReturnType]);

  if (returnType == MLKT_INVALID)
    {
      [NSException raise:@"MLKInvalidReturnTypeError"
                   format:@"Cannot handle an Objective-C return type of \"%s\" \
as provided by method %@ of object %@",
                          methodName, object, [signature methodReturnType]];
      return nil;
    }
  else if (returnType == MLKT_VOID)
    {
      return [NSArray array];
    }
  else
    {
      ffi_type *ffi_rettype = MLKFFITypeWithForeignType (returnType);
      void *returnValue = alloca (ffi_rettype->size);
      [invocation getReturnValue:returnValue];
      RETURN_VALUE (MLKLispValueWithForeignValue (returnValue, returnType));
    }
}


+(NSArray *) declarations_and_doc_and_forms:(NSArray *)args
{
  id decls, doc, forms;
  id bodyAndDecls = denullify ([args objectAtIndex:0]);

  MLKSplitDeclarationsDocAndForms (&decls, &doc, &forms, bodyAndDecls, YES);

  RETURN_VALUE ([MLKCons
                  cons:decls
                  with:[MLKCons
                         cons:doc
                         with:[MLKCons
                                cons:forms
                                with:nil]]]);
}


+(NSArray *) declarations_and_forms:(NSArray *)args
{
  id decls, doc, forms;
  id bodyAndDecls = denullify ([args objectAtIndex:0]);
  
  MLKSplitDeclarationsDocAndForms (&decls, &doc, &forms, bodyAndDecls, NO);
  
  RETURN_VALUE ([MLKCons
                  cons:decls
                  with:[MLKCons
                         cons:forms
                         with:nil]]);
}

+(NSArray *) compile:(NSArray *)args
{
  if (!MLKDefaultCompiler)
    [NSException raise:@"MLKNotImplementedException"
                 format:@"It seems as though there is no compiler here."];

  //NSLog (@"Compiling lambda form.");
  id thing = [MLKDefaultCompiler compile:denullify([args objectAtIndex:0])
                                 inContext:[MLKLexicalContext globalContext]];
  //NSLog (@"Compilation done.");
  //NSLog (@"Compiled: %@", thing);
  RETURN_VALUE (thing);
}

+(NSArray *) fset:(NSArray *)args
{
  id symbol = denullify ([args objectAtIndex:0]);
  id value = denullify ([args objectAtIndex:1]);

  [[MLKLexicalContext globalContext] addFunction:symbol];
  [[MLKLexicalEnvironment globalEnvironment] addFunction:value
                                             forSymbol:symbol];

  RETURN_VALUE (value);
}

+(NSArray *) set:(NSArray *)args
{
  id symbol = denullify ([args objectAtIndex:0]);
  id value = denullify ([args objectAtIndex:1]);
  MLKDynamicContext *dynamicContext = [MLKDynamicContext currentContext];

  if ([dynamicContext bindingForSymbol:symbol])
    [dynamicContext setValue:value forSymbol:symbol];
  else
    [[MLKDynamicContext globalContext] addValue:value
                                       forSymbol:symbol];

  RETURN_VALUE (value);
}

+(NSArray *) macroset:(NSArray *)args
{
  id symbol = denullify ([args objectAtIndex:0]);
  id value = denullify ([args objectAtIndex:1]);

  [[MLKLexicalContext globalContext] addMacro:value
                                     forSymbol:symbol];

  RETURN_VALUE (value);
}

+(NSArray *) apply:(NSArray *)args
{
  id function = denullify ([args objectAtIndex:0]);
  id arglist = denullify ([args objectAtIndex:1]);

  if (!function || [function isKindOfClass:[MLKSymbol class]])
    {
      function = [[MLKLexicalEnvironment globalEnvironment]
                   functionForSymbol:function];
    }

  return [function applyToArray:(arglist
                                 ? (id)[arglist array]
                                 : (id)[NSArray array])];
}

+(NSArray *) eval:(NSArray *)args
{
  id evaluand = denullify ([args objectAtIndex:0]);
  return [MLKInterpreter eval:evaluand
             inLexicalContext:[MLKLexicalContext globalContext]
              withEnvironment:[MLKLexicalEnvironment globalEnvironment]];
}
@end
