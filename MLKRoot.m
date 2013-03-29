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

#import "MLKBinaryStreamCharacterStream.h"
#import "MLKBinding.h"
#import "MLKCharacter.h"
#import "MLKCompiledClosure.h"
#import "MLKCons.h"
#import "MLKDynamicContext.h"
#import "MLKInterpretedClosure.h"
#import "MLKInterpreter.h"
#import "MLKLLVMCompiler.h"
#import "MLKNumber.h"
#import "MLKPackage.h"
#import "MLKRoot.h"
#import "MLKStreamStream.h"
#import "MLKSymbol.h"
#import "MLKInteger.h"
#import "MLKSingleFloat.h"
#import "MLKDoubleFloat.h"
#import "NSObject-MLKPrinting.h"
#import "globals.h"
#import "runtime-compatibility.h"
#import "util.h"

#import <Foundation/NSArray.h>
#import <Foundation/NSBundle.h>
#import <Foundation/NSException.h>
#import <Foundation/NSInvocation.h>
#import <Foundation/NSMethodSignature.h>
#import <Foundation/NSNull.h>
#import <Foundation/NSPathUtilities.h>
#import <Foundation/NSStream.h>
#import <Foundation/NSString.h>

#include <alloca.h>


static NSMethodSignature *signature;
static MLKPackage *sys;
static MLKPackage *cl;
static NSMutableDictionary *lisp_c_name_map;


static id
truify (BOOL value)
{
  return (value ? (id) [cl intern:@"T"] : nil);
}


id
toilet_car (id *_data, id *_multireturn, id cons, id _marker)
{
  return [cons car];
}

id
toilet_cdr (id *_data, id *_multireturn, id cons, id _marker)
{
  return [cons cdr];
}

id
toilet_rplaca (id *_data, id *_multireturn, id cons, id value, id _marker)
{
  [cons setCar:value];
  return cons;
}

id
toilet_rplacd (id *_data, id *_multireturn, id cons, id value, id _marker)
{
  [cons setCdr:value];
  return cons;
}

id
toilet_cons (id *_data, id *_multireturn, id car, id cdr, id _marker)
{
  return [MLKCons cons:car with:cdr];
}

id
toilet_load (id *_data, id *_multireturn, NSString *fileName, id _marker)
{
  BOOL success;
  int l, i;
  NSInputStream *input = [NSInputStream inputStreamWithFileAtPath:fileName];
  MLKBinaryStream *filestream = LAUTORELEASE ([[MLKStreamStream alloc]
                                                initWithInputStream:input]);
  MLKCharacterStream *stream = LAUTORELEASE ([[MLKBinaryStreamCharacterStream alloc]
                                               initWithBinaryStream:filestream]);
  MLKDynamicContext *oldContext = [MLKDynamicContext currentContext];
  int level = MLKIntWithInteger ([oldContext
                                   valueForSymbol:[sys intern:@"*LOAD-LEVEL*"]]);
  MLKDynamicContext *ctx;

  l = [fileName length];

  MLKCharacterStream *ostream = [[MLKDynamicContext currentContext]
                                  valueForSymbol:[cl intern:@"*STANDARD-OUTPUT*"]];

  [ostream writeString:@";\n;  "];
  for (i = 0; i < 68 - 2*level; i++)
    [ostream writeChar:'_'];

  [ostream writeString:@"\n; /"];
  for (i = 0; i < 30 - l/2 - level; i++)
    [ostream writeChar:'-'];
  [ostream writeString:[NSString stringWithFormat:@" LOAD: %s ", [fileName UTF8String]]];
  for (i = 0; i < 30 - (l+1)/2 - level; i++)
    [ostream writeChar:'-'];
  [ostream writeString:@"\n; |\n"];

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

  @try
    {
      success = [MLKInterpreter load:stream verbose:YES print:YES];
    }
  @finally
    {
      [MLKDynamicContext popContext];
      LRELEASE (ctx);
      [input close];
    }

  [ostream writeString:@"; \\"];
  for (i = 0; i < 68 - 2*level; i++)
    [ostream writeChar:'_'];
  [ostream writeString:@"\n; \n"];

  return truify (success);
}

id
toilet_require (id *_data, id *_multireturn, id moduleName, id _marker)
{
  NSLog(@"require...");
  NSBundle *toiletKit = [NSBundle bundleForClass:[MLKRoot class]];
  NSString *path = [[toiletKit resourcePath]
                    stringByAppendingPathComponent:stringify(moduleName)];
  return toilet_load (NULL, _multireturn, path, MLKEndOfArgumentsMarker);
}

id
toilet_eq (id *_data, id *_multireturn, id x, id y, id _marker)
{
  return truify (x == y);
}

id
toilet_fixnum_eq (id *_data, id *_multireturn, id x, id y, id _marker)
{
#ifdef NO_FIXNUMS
  return truify ([x isEqual:y]);
#else
  return truify (x == y);
#endif
}

id
toilet_symbolp (id *_data, id *_multireturn, id arg0, id _marker)
{
  return truify (MLKInstanceP(arg0)
                 && (!arg0 || [arg0 isKindOfClass:[MLKSymbol class]]));
}

id
toilet_listp (id *_data, id *_multireturn, id arg0, id _marker)
{
  return truify (MLKInstanceP(arg0)
                 && (!arg0 || [arg0 isKindOfClass:[MLKCons class]]));
}

id
toilet_consp (id *_data, id *_multireturn, id arg0, id _marker)
{
  return truify (MLKInstanceP(arg0)
                 && [arg0 isKindOfClass:[MLKCons class]]);
}

id
toilet_atom (id *_data, id *_multireturn, id arg0, id _marker)
{
  return truify (!MLKInstanceP(arg0)
                 || ![arg0 isKindOfClass:[MLKCons class]]);
}

id
toilet_null (id *_data, id *_multireturn, id arg0, id _marker)
{
  return truify (!arg0);
}

id
toilet_fixnump (id *_data, id *_multireturn, id arg0, id _marker)
{
  return truify (MLKFixnumP(arg0));
}

id
toilet_add (id *_data, id *_multireturn, MLKNumber *x, MLKNumber *y, id _marker)
{
  return [nullify(x) add:nullify(y)];
}

id
toilet_subtract (id *_data, id *_multireturn, MLKNumber *x, MLKNumber *y, id _marker)
{
  return [nullify(x) subtract:nullify(y)];
}

id
toilet_multiply (id *_data, id *_multireturn, MLKNumber *x, MLKNumber *y, id _marker)
{
  return [nullify(x) multiplyWith:nullify(y)];
}

id
toilet_divide (id *_data, id *_multireturn, MLKNumber *x, MLKNumber *y, id _marker)
{
  return [nullify(x) divideBy:nullify(y)];
}

id
toilet_add_fixnums (id *_data, id *_multireturn, id x, id y, id _marker)
{
  return MLKAddFixnums (x, y);
}

id
toilet_subtract_fixnums (id *_data, id *_multireturn, id x, id y, id _marker)
{
  return MLKSubtractFixnums (x, y);
}

id
toilet_idivide_fixnums (id *_data, id *_multireturn, id x, id y, id _marker)
{
  return MLKIDivideFixnums (x, y);
}

id
toilet_multiply_fixnums (id *_data, id *_multireturn, id x, id y, id _marker)
{
  return MLKMultiplyFixnums (x, y);
}

id
toilet_list (id *_data, id *_multireturn, ...)
{
  id arg;
  va_list ap;
  id cons, tail;

  cons = nil;
  tail = nil;
  va_start (ap, _multireturn);

  while ((arg = va_arg(ap, id)) != MLKEndOfArgumentsMarker)
    {
      //NSLog (@"list: Adding stuff (%%p = %p).", arg);
      //NSLog (@"list: Stuff: %p = %@", arg, nullify(arg));
      if (!tail)
        {
          cons = tail = [MLKCons cons:arg with:nil];
        }
      else
        {
          [tail setCdr:[MLKCons cons:arg with:nil]];
          tail = [tail cdr];
        }
    }

  va_end (ap);

  //NSLog (@"list: Done.  Result: %p", cons);
  //NSLog (@"list: %p = %@", cons, cons);
  return cons;
}

#define VA_NEXT(AP, ARG, DEFAULT)                               \
  ((ARG == MLKEndOfArgumentsMarker)                             \
   ? (id)DEFAULT                                                \
   : (id)({ id __tmp = ARG; ARG = va_arg(AP, id); __tmp; }))

id
toilet_macroexpand_1 (id *_data, id *_multireturn, id form, id arg, ...)
{
  va_list ap;

  va_start (ap, arg);
  MLKLexicalContext *context = VA_NEXT (ap, arg, nil);
  id <MLKFuncallable> macrofun = nil;
  va_end (ap);

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

  return form;
}

id
toilet_shadow_ (id *_data, id *_multireturn, id symbols, id arg, ...)
{
  va_list ap;

  va_start (ap, arg);
  id package = VA_NEXT (ap, arg, [[MLKDynamicContext currentContext]
                                   valueForSymbol:
                                     [[MLKPackage findPackage:@"COMMON-LISP"]
                                       intern:@"*PACKAGE*"]]);
  va_end (ap);

  if (![symbols isKindOfClass:[MLKCons class]])
    symbols = [MLKCons cons:symbols with:nil];

  do
    {
      [package shadow:stringify([symbols car])];
    }
  while ((symbols = [symbols cdr]));

  return [cl intern:@"T"];
}

id
toilet_export (id *_data, id *_multireturn, id symbols, id arg, ...)
{
  va_list ap;

  va_start (ap, arg);
  id package = VA_NEXT (ap, arg, [[MLKDynamicContext currentContext]
                                   valueForSymbol:
                                     [[MLKPackage findPackage:@"COMMON-LISP"]
                                       intern:@"*PACKAGE*"]]);
  va_end (ap);

  if (![symbols isKindOfClass:[MLKCons class]])
    symbols = [MLKCons cons:symbols with:nil];

  do
    {
      [package export:[symbols car]];
    }
  while ((symbols = [symbols cdr]));

  return [cl intern:@"T"];
}

id
toilet_unexport (id *_data, id *_multireturn, id symbols, id arg, ...)
{
  va_list ap;

  va_start (ap, arg);
  id package = VA_NEXT (ap, arg, [[MLKDynamicContext currentContext]
                                   valueForSymbol:
                                     [[MLKPackage findPackage:@"COMMON-LISP"]
                                       intern:@"*PACKAGE*"]]);
  va_end (ap);

  if (![symbols isKindOfClass:[MLKCons class]])
    symbols = [MLKCons cons:symbols with:nil];

  do
    {
      [package unexport:[symbols car]];
    }
  while ((symbols = [symbols cdr]));

  return [cl intern:@"T"];
}

id
toilet_find_package (id *_data, id *_multireturn, id name, id _marker)
{
  MLKPackage *package = [MLKPackage findPackage:stringify(name)];

  if (package)
    {
      return package;
    }
  else
    {
      [NSException raise:@"MLKNoSuchPackageError"
                   format:@"The package %@ does not exist",
                          MLKPrintToString(name)];
      return nil;
    }
}

id
toilet_string (id *_data, id *_multireturn, id x, id _marker)
{
  return stringify (x);
}

id
toilet_gensym (id *_data, id *_multireturn, id arg, ...)
{
  va_list ap;

  va_start (ap, arg);
  id x = VA_NEXT (ap, arg, @"G");
  va_end (ap);

  NSString *prefix;
  NSString *suffix;
  MLKBinding *gensymCounter = [[MLKDynamicContext currentContext]
                                bindingForSymbol:
                                  [[MLKPackage findPackage:@"COMMON-LISP"]
                                    intern:@"*GENSYM-COUNTER*"]];

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

  return [MLKSymbol symbolWithName:
                      [NSString stringWithFormat:@"%@%@", prefix, suffix]
                    package:nil];
}

id
toilet_make_symbol (id *_data, id *_multireturn, id name, id _marker)
{
  return [MLKSymbol symbolWithName:name package:nil];
}

id
toilet_intern (id *_data, id *_multireturn, id name, id arg, ...)
{
  va_list ap;

  va_start (ap, arg);
  id package = VA_NEXT (ap, arg, [[MLKDynamicContext currentContext]
                                   valueForSymbol:
                                     [[MLKPackage findPackage:@"COMMON-LISP"]
                                       intern:@"*PACKAGE*"]]);
  va_end (ap);

  return [package intern:name];
}

id
toilet_import (id *_data, id *_multireturn, id symbol, id arg, ...)
{
  va_list ap;

  va_start (ap, arg);
  id package = VA_NEXT (ap, arg, [[MLKDynamicContext currentContext]
                                   valueForSymbol:
                                     [[MLKPackage findPackage:@"COMMON-LISP"]
                                       intern:@"*PACKAGE*"]]);
  va_end (ap);

  [package import:symbol];

  return [cl intern:@"T"];
}

id
toilet_objc_class_of (id *_data, id *_multireturn, id x, id _marker)
{
  return [x class];
}

id
toilet_objc_subclassp (id *_data, id *_multireturn, id x, id y, id _marker)
{
  return truify ([x isSubclassOfClass:y]);
}

id
toilet_find_objc_class (id *_data, id *_multireturn, id x, id _marker)
{
  return NSClassFromString (x);
}

id
toilet_ns_log (id *_data, id *_multireturn, id x, id _marker)
{
  NSString *description = MLKPrintToString(x);
  NSLog (@"%@", description);
  return x;
}

id
toilet_symbol_name (id *_data, id *_multireturn, id symbol, id _marker)
{
  return (symbol ? (id)[symbol name] : (id)@"NIL");
}

id
toilet_primitive_type_of (id *_data, id *_multireturn, id object, id _marker)
{
  if (!object)
    { return [cl intern:@"NULL"]; }
  else if (MLKFixnumP (object))
    { return [cl intern:@"FIXNUM"]; }
  else if ([object isKindOfClass:[MLKSymbol class]])
    { return [cl intern:@"SYMBOL"]; }
  else if ([object isKindOfClass:[MLKCons class]])
    { return [cl intern:@"CONS"]; }
  else if ([object isKindOfClass:[MLKDoubleFloat class]])
    { return [cl intern:@"DOUBLE-FLOAT"]; }
  else if ([object isKindOfClass:[MLKSingleFloat class]])
    { return [cl intern:@"SINGLE-FLOAT"]; }
  else if ([object isKindOfClass:[MLKInteger class]])
    { return [cl intern:@"INTEGER"]; }
  else if ([object isKindOfClass:[MLKCharacter class]])
    //FIXME: STANDARD-CHAR
    { return [cl intern:@"BASE-CHAR"]; }
  else if ([object isKindOfClass:[MLKInterpretedClosure class]])
    { return [cl intern:@"FUNCTION"]; }
  else if ([object isKindOfClass:[MLKLexicalContext class]])
    { return [sys intern:@"LEXICAL-CONTEXT"]; }
  else if ([object isKindOfClass:[MLKBinding class]])
    { return [sys intern:@"BINDING"]; }
  else if ([object isKindOfClass:[MLKPackage class]])
    { return [cl intern:@"PACKAGE"]; }
  else if ([object isKindOfClass:[MLKBinaryStream class]])
    { return [cl intern:@"BINARY-STREAM"]; }
  else if ([object isKindOfClass:[MLKCharacterStream class]])
    { return [cl intern:@"CHARACTER-STREAM"]; }
  else if ([object isKindOfClass:[NSException class]])
    { return [sys intern:@"EXCEPTION"]; }
  else if ([object isKindOfClass:[NSArray class]])
    { return [cl intern:@"ARRAY"]; }
  else
    { return [cl intern:@"T"]; }
}

id
toilet_send_by_name (id *_data, id *_multireturn, id object, NSString *methodName, id arg, ...)
{
  NSInvocation *invocation;
  SEL selector;
  NSMethodSignature *signature;
  int i;
  MLKForeignType returnType;
  va_list ap;

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

  i = 2;
  va_start (ap, arg);
  while (arg != MLKEndOfArgumentsMarker)
    {
      const char *objctype = [signature getArgumentTypeAtIndex:i];
      MLKForeignType type = MLKForeignTypeWithObjectiveCType (objctype);
      ffi_type *ffi_argtype = MLKFFITypeWithForeignType (type);
      void *argbuf = alloca (ffi_argtype->size);

      if (type == MLKT_INVALID)
        [NSException raise:@"MLKInvalidArgumentError"
                     format:@"Don't know how to coerce %@ into type \"%s\".",
                     arg, objctype];

      MLKSetForeignValueWithLispValue (argbuf, arg, type);
      [invocation setArgument:argbuf atIndex:i];

      arg = va_arg (ap, id);
      i++;
    }
  va_end (ap);

  [invocation invoke];


  returnType = MLKForeignTypeWithObjectiveCType ([signature methodReturnType]);

  if (returnType == MLKT_INVALID)
    {
      [NSException raise:@"MLKInvalidReturnTypeError"
                   format:@"Cannot handle an Objective-C return type of \"%@\" \
as provided by method %@ of object %s",
                          methodName, object, [signature methodReturnType]];
      return nil;
    }
  else if (returnType == MLKT_VOID)
    {
      return nil;
    }
  else
    {
      ffi_type *ffi_rettype = MLKFFITypeWithForeignType (returnType);
      void *returnValue = alloca (ffi_rettype->size);
      [invocation getReturnValue:returnValue];
      return MLKLispValueWithForeignValue (returnValue, returnType);
    }
}


id
toilet_declarations_and_doc_and_forms (id *_data, id *_multireturn, id bodyAndDecls, id _marker)
{
  id decls, doc, forms;

  MLKSplitDeclarationsDocAndForms (&decls, &doc, &forms, bodyAndDecls, YES);

  return [MLKCons cons:decls
                  with:[MLKCons cons:doc
                                with:[MLKCons cons:forms with:nil]]];
}


id
toilet_declarations_and_forms (id *_data, id *_multireturn, id bodyAndDecls, id _marker)
{
  id decls, doc, forms;
  
  MLKSplitDeclarationsDocAndForms (&decls, &doc, &forms, bodyAndDecls, NO);
  
  return [MLKCons cons:decls
                  with:[MLKCons cons:forms with:nil]];
}

id
toilet_compile (id *_data, id *_multireturn, id object, id _marker)
{
  if (!MLKDefaultCompiler)
    [NSException raise:@"MLKNotImplementedException"
                 format:@"It seems as though there is no compiler here."];

  //NSLog (@"Compiling lambda form.");
  id thing = [MLKDefaultCompiler compile:object
                                 inContext:[MLKLexicalContext globalContext]];
  //NSLog (@"Compilation done.");
  //NSLog (@"Compiled: %@", thing);
  return thing;
}

id
toilet_fset (id *_data, id *_multireturn, id symbol, id value, id _marker)
{
  [[MLKLexicalContext globalContext] addFunction:symbol];
  [[MLKLexicalEnvironment globalEnvironment] addFunction:value
                                             forSymbol:symbol];

  return value;
}

id
toilet_set (id *_data, id *_multireturn, id symbol, id value, id _marker)
{
  MLKDynamicContext *dynamicContext = [MLKDynamicContext currentContext];

  if ([dynamicContext bindingForSymbol:symbol])
    [dynamicContext setValue:value forSymbol:symbol];
  else
    [[MLKDynamicContext globalContext] addValue:value
                                       forSymbol:symbol];

  return value;
}

id
toilet_macroset (id *_data, id *_multireturn, id symbol, id value, id _marker)
{
  [[MLKLexicalContext globalContext] addMacro:value
                                     forSymbol:symbol];

  return value;
}

id
toilet_apply (id *_data, id *_multireturn, id function, id arglist, id _marker)
{
  // FIXME: Multiple values.

  if (!function || [function isKindOfClass:[MLKSymbol class]])
    {
      function = [[MLKLexicalEnvironment globalEnvironment]
                   functionForSymbol:function];
    }

  NSArray *values = [function applyToArray:(arglist
                                            ? (id)[arglist array]
                                            : (id)[NSArray array])];

  return ([values count] > 0 ? denullify([values objectAtIndex:0]) : nil);
}

id
toilet_eval (id *_data, id *_multireturn, id evaluand, id _marker)
{
  // FIXME: Multiple values.

  NSArray *values = [MLKInterpreter eval:evaluand
                                    inLexicalContext:
                                      [MLKLexicalContext globalContext]
                                    withEnvironment:
                                      [MLKLexicalEnvironment globalEnvironment]];

  return ([values count] > 0 ? denullify([values objectAtIndex:0]) : nil);
}

static void
register_cl (NSString *name, NSString *c_name, id (*function)())
{
  MLKCompiledClosure *closure = [MLKCompiledClosure closureWithCode:function
                                                               data:NULL
                                                             length:0];
  [[MLKLexicalContext globalContext]
    addFunction:[cl intern:name]];
  [[MLKLexicalEnvironment globalEnvironment]
    addFunction:closure
      forSymbol:[cl intern:name]];
  [lisp_c_name_map setObject:c_name forKey:[cl intern:name]];
}

static void
register_sys (NSString *name, NSString *c_name, id (*function)())
{
  MLKCompiledClosure *closure = [MLKCompiledClosure closureWithCode:function
                                                               data:NULL
                                                             length:0];
  [[MLKLexicalContext globalContext]
    addFunction:[sys intern:name]];
  [[MLKLexicalEnvironment globalEnvironment]
    addFunction:closure
      forSymbol:[sys intern:name]];
  [lisp_c_name_map setObject:c_name forKey:[sys intern:name]];
}

const char *toilet_built_in_function_name(id lisp_name)
{
  NSString *name = [lisp_c_name_map objectForKey:lisp_name];
  if (name) {
    return [name UTF8String];
  } else {
    return NULL;
  }
}

@implementation MLKRoot
+(void) initialize
{
  lisp_c_name_map = [[NSMutableDictionary alloc] init];

  signature = LRETAIN ([self methodSignatureForSelector:@selector(car:)]);
  sys = [MLKPackage findPackage:@"TOILET-SYSTEM"];
  cl = [MLKPackage findPackage:@"COMMON-LISP"];

  // NOTE TO SELF: PREFIX NAMES WITH TOILET_ AND UN-STATIC-IZE THEM SO THAT
  // LLVM MAY FIND AND INVOKE THEM!

  register_sys (@"CAR", @"toilet_car", toilet_car);
  register_sys (@"CDR", @"toilet_cdr", toilet_cdr);
  register_sys (@"RPLACA", @"toilet_rplaca", toilet_rplaca);
  register_sys (@"RPLACD", @"toilet_rplacd", toilet_rplacd);
  register_sys (@"CONS", @"toilet_cons", toilet_cons);
  register_sys (@"LOAD", @"toilet_load", toilet_load);
  register_sys (@"REQUIRE", @"toilet_require", toilet_require);
  register_sys (@"EQ", @"toilet_eq", toilet_eq);
  register_sys (@"FIXNUM-EQ", @"toilet_fixnum_eq", toilet_fixnum_eq);
  register_sys (@"SYMBOLP", @"toilet_symbolp", toilet_symbolp);
  register_sys (@"LISTP", @"toilet_listp", toilet_listp);
  register_sys (@"CONSP", @"toilet_consp", toilet_consp);
  register_sys (@"ATOM", @"toilet_atom", toilet_atom);
  register_sys (@"NULL", @"toilet_null", toilet_null);
  register_sys (@"FIXNUMP", @"toilet_fixnump", toilet_fixnump);
  register_sys (@"ADD", @"toilet_add", toilet_add);
  register_sys (@"SUBTRACT", @"toilet_subtract", toilet_subtract);
  register_sys (@"MULTIPLY", @"toilet_multiply", toilet_multiply);
  register_sys (@"DIVIDE", @"toilet_divide", toilet_divide);
  register_sys (@"ADD-FIXNUMS", @"toilet_add_fixnums", toilet_add_fixnums);
  register_sys (@"SUBTRACT-FIXNUMS", @"toilet_subtract_fixnums", toilet_subtract_fixnums);
  register_sys (@"MULTIPLY-FIXNUMS", @"toilet_multiply_fixnums", toilet_multiply_fixnums);
  register_sys (@"IDIVIDE-FIXNUMS", @"toilet_idivide_fixnums", toilet_idivide_fixnums);
  register_sys (@"LIST", @"toilet_list", (id (*)())toilet_list);
  register_sys (@"MACROEXPAND-1", @"macroexpand_1", (id (*)())toilet_macroexpand_1);
  register_sys (@"SHADOW", @"toilet_shadow_", (id (*)())toilet_shadow_);
  register_sys (@"EXPORT", @"toilet_export", (id (*)())toilet_export);
  register_sys (@"UNEXPORT", @"toilet_unexport", (id (*)())toilet_unexport);
  register_sys (@"FIND-PACKAGE", @"toilet_find_package", toilet_find_package);
  register_sys (@"STRING", @"toilet_string", toilet_string);
  register_sys (@"GENSYM", @"toilet_gensym", (id (*)())toilet_gensym);
  register_sys (@"MAKE-SYMBOL", @"toilet_make_symbol", toilet_make_symbol);
  register_sys (@"INTERN", @"toilet_intern", (id (*)())toilet_intern);
  register_sys (@"IMPORT", @"toilet_import", (id (*)())toilet_import);
  register_sys (@"OBJC-CLASS-OF", @"toilet_objc_class_of", toilet_objc_class_of);
  register_sys (@"OBJC-SUBCLASSP", @"toilet_objc_subclassp", toilet_objc_subclassp);
  register_sys (@"FIND-OBJC-CLASS", @"toilet_find_objc_class", toilet_find_objc_class);
  register_sys (@"NS-LOG", @"toilet_ns_log", toilet_ns_log);
  register_sys (@"SYMBOL-NAME", @"toilet_symbol_name", toilet_symbol_name);
  register_sys (@"PRIMITIVE-TYPE-OF", @"toilet_primitive_type_of", toilet_primitive_type_of);
  register_sys (@"SEND-BY-NAME", @"toilet_send_by_name", (id (*)())toilet_send_by_name);
  register_sys (@"DECLARATIONS-AND-DOC-AND-FORMS", @"toilet_declarations_and_doc_and_forms", toilet_declarations_and_doc_and_forms);
  register_sys (@"DECLARATIONS-AND-FORMS", @"toilet_declarations_and_forms", toilet_declarations_and_forms);
  register_sys (@"COMPILE", @"toilet_compile", toilet_compile);
  register_sys (@"%FSET", @"toilet_fset", toilet_fset);
  register_sys (@"SET", @"toilet_set", toilet_set);
  register_sys (@"%MACROSET", @"toilet_macroset", toilet_macroset);
  register_sys (@"APPLY", @"toilet_apply", toilet_apply);
  register_sys (@"EVAL", @"toilet_eval", toilet_eval);
}

+(void) registerBuiltins
{
  // Do the real work in +initialize.
}
@end
