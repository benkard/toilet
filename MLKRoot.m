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


static id truify (BOOL value)
{
  return (value ? (id) [cl intern:@"T"] : nil);
}


static id
car (id *_data, id cons, id _marker)
{
  return [cons car];
}

static id
cdr (id *_data, id cons, id _marker)
{
  return [cons cdr];
}

static id
rplaca (id *_data, id cons, id value, id _marker)
{
  [cons setCar:value];
  return cons;
}

static id
rplacd (id *_data, id cons, id value, id _marker)
{
  [cons setCdr:value];
  return cons;
}

static id
cons (id *_data, id car, id cdr, id _marker)
{
  return [MLKCons cons:car with:cdr];
}

static id
load (id *_data, NSString *fileName, id _marker)
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

static id
require (id *_data, id moduleName, id _marker)
{
  NSBundle *toiletKit = [NSBundle bundleForClass:[MLKRoot class]];
  NSString *path = [[toiletKit resourcePath]
                    stringByAppendingPathComponent:stringify(moduleName)];
  return load (NULL, path, MLKEndOfArgumentsMarker);
}

static id
eq (id *_data, id x, id y, id _marker)
{
  return truify (x == y);
}

static id
fixnum_eq (id *_data, id x, id y, id _marker)
{
#ifdef NO_FIXNUMS
  return truify ([x isEqual:y]);
#else
  return truify (x == y);
#endif
}

static id
symbolp (id *_data, id arg0, id _marker)
{
  return truify (MLKInstanceP(arg0)
                 && (!arg0 || [arg0 isKindOfClass:[MLKSymbol class]]));
}

static id
listp (id *_data, id arg0, id _marker)
{
  return truify (MLKInstanceP(arg0)
                 && (!arg0 || [arg0 isKindOfClass:[MLKCons class]]));
}

static id
consp (id *_data, id arg0, id _marker)
{
  return truify (MLKInstanceP(arg0)
                 && [arg0 isKindOfClass:[MLKCons class]]);
}

static id
atom (id *_data, id arg0, id _marker)
{
  return truify (!MLKInstanceP(arg0)
                 || ![arg0 isKindOfClass:[MLKCons class]]);
}

static id
null (id *_data, id arg0, id _marker)
{
  return truify (!arg0);
}

static id
fixnump (id *_data, id arg0, id _marker)
{
  return truify (MLKFixnumP(arg0));
}

static id
add (id *_data, MLKNumber *x, MLKNumber *y, id _marker)
{
  return [nullify(x) add:nullify(y)];
}

static id
subtract (id *_data, MLKNumber *x, MLKNumber *y, id _marker)
{
  return [nullify(x) subtract:nullify(y)];
}

static id
multiply (id *_data, MLKNumber *x, MLKNumber *y, id _marker)
{
  return [nullify(x) multiplyWith:nullify(y)];
}

static id
divide (id *_data, MLKNumber *x, MLKNumber *y, id _marker)
{
  return [nullify(x) divideBy:nullify(y)];
}

static id
add_fixnums (id *_data, id x, id y, id _marker)
{
  return MLKAddFixnums (x, y);
}

static id
subtract_fixnums (id *_data, id x, id y, id _marker)
{
  return MLKSubtractFixnums (x, y);
}

static id
idivide_fixnums (id *_data, id x, id y, id _marker)
{
  return MLKIDivideFixnums (x, y);
}

static id
multiply_fixnums (id *_data, id x, id y, id _marker)
{
  return MLKMultiplyFixnums (x, y);
}

static id
list (id *_data, ...)
{
  id arg;
  va_list ap;
  id cons, tail;

  cons = nil;
  tail = nil;
  va_start (ap, _data);

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

static id
macroexpand_1 (id *_data, id form, id arg, ...)
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

static id
shadow_ (id *_data, id symbols, id arg, ...)
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

static id
export (id *_data, id symbols, id arg, ...)
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

static id
unexport (id *_data, id symbols, id arg, ...)
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

static id
find_package (id *_data, id name, id _marker)
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

static id
string (id *_data, id x, id _marker)
{
  return stringify (x);
}

static id
gensym (id *_data, id arg, ...)
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

static id
make_symbol (id *_data, id name, id _marker)
{
  return [MLKSymbol symbolWithName:name package:nil];
}

static id
intern (id *_data, id name, id arg, ...)
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

static id
import (id *_data, id symbol, id arg, ...)
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

static id
objc_class_of (id *_data, id x, id _marker)
{
  return [x class];
}

static id
objc_subclassp (id *_data, id x, id y, id _marker)
{
  return truify ([x isSubclassOfClass:y]);
}

static id
find_objc_class (id *_data, id x, id _marker)
{
  return NSClassFromString (x);
}

static id
ns_log (id *_data, id x, id _marker)
{
  NSString *description = MLKPrintToString(x);
  NSLog (@"%@", description);
  return x;
}

static id
symbol_name (id *_data, id symbol, id _marker)
{
  return (symbol ? (id)[symbol name] : (id)@"NIL");
}

static id
primitive_type_of (id *_data, id object, id _marker)
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

static id
send_by_name (id *_data, id object, NSString *methodName, id arg, ...)
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
                   format:@"Cannot handle an Objective-C return type of \"%s\" \
as provided by method %@ of object %@",
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


static id
declarations_and_doc_and_forms (id *_data, id bodyAndDecls, id _marker)
{
  id decls, doc, forms;

  MLKSplitDeclarationsDocAndForms (&decls, &doc, &forms, bodyAndDecls, YES);

  return [MLKCons cons:decls
                  with:[MLKCons cons:doc
                                with:[MLKCons cons:forms with:nil]]];
}


static id
declarations_and_forms (id *_data, id bodyAndDecls, id _marker)
{
  id decls, doc, forms;
  
  MLKSplitDeclarationsDocAndForms (&decls, &doc, &forms, bodyAndDecls, NO);
  
  return [MLKCons cons:decls
                  with:[MLKCons cons:forms with:nil]];
}

static id
compile (id *_data, id object, id _marker)
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

static id
fset (id *_data, id symbol, id value, id _marker)
{
  [[MLKLexicalContext globalContext] addFunction:symbol];
  [[MLKLexicalEnvironment globalEnvironment] addFunction:value
                                             forSymbol:symbol];

  return value;
}

static id
set (id *_data, id symbol, id value, id _marker)
{
  MLKDynamicContext *dynamicContext = [MLKDynamicContext currentContext];

  if ([dynamicContext bindingForSymbol:symbol])
    [dynamicContext setValue:value forSymbol:symbol];
  else
    [[MLKDynamicContext globalContext] addValue:value
                                       forSymbol:symbol];

  return value;
}

static id
macroset (id *_data, id symbol, id value, id _marker)
{
  [[MLKLexicalContext globalContext] addMacro:value
                                     forSymbol:symbol];

  return value;
}

static id
apply (id *_data, id function, id arglist, id _marker)
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

  return ([values count] > 0 ? [values objectAtIndex:0] : nil);
}

static id
eval (id *_data, id evaluand, id _marker)
{
  // FIXME: Multiple values.

  NSArray *values = [MLKInterpreter eval:evaluand
                                    inLexicalContext:
                                      [MLKLexicalContext globalContext]
                                    withEnvironment:
                                      [MLKLexicalEnvironment globalEnvironment]];

  return ([values count] > 0 ? [values objectAtIndex:0] : nil);
}

static void
register_cl (NSString *name, id (*function)())
{
  MLKCompiledClosure *closure = [MLKCompiledClosure closureWithCode:function
                                                               data:NULL
                                                             length:0];
  [[MLKLexicalContext globalContext]
    addFunction:[cl intern:name]];
  [[MLKLexicalEnvironment globalEnvironment]
    addFunction:closure
      forSymbol:[cl intern:name]];
}

static void
register_sys (NSString *name, id (*function)())
{
  MLKCompiledClosure *closure = [MLKCompiledClosure closureWithCode:function
                                                               data:NULL
                                                             length:0];
  [[MLKLexicalContext globalContext]
    addFunction:[sys intern:name]];
  [[MLKLexicalEnvironment globalEnvironment]
    addFunction:closure
      forSymbol:[sys intern:name]]; 
}

@implementation MLKRoot
+(void) initialize
{
  signature = LRETAIN ([self methodSignatureForSelector:@selector(car:)]);
  sys = [MLKPackage findPackage:@"TOILET-SYSTEM"];
  cl = [MLKPackage findPackage:@"COMMON-LISP"];

  register_sys (@"CAR", car);
  register_sys (@"CDR", cdr);
  register_sys (@"RPLACA", rplaca);
  register_sys (@"RPLACD", rplacd);
  register_sys (@"CONS", cons);
  register_sys (@"LOAD", load);
  register_sys (@"REQUIRE", require);
  register_sys (@"EQ", eq);
  register_sys (@"FIXNUM-EQ", fixnum_eq);
  register_sys (@"SYMBOLP", symbolp);
  register_sys (@"LISTP", listp);
  register_sys (@"CONSP", consp);
  register_sys (@"ATOM", atom);
  register_sys (@"NULL", null);
  register_sys (@"FIXNUMP", fixnump);
  register_sys (@"ADD", add);
  register_sys (@"SUBTRACT", subtract);
  register_sys (@"MULTIPLY", multiply);
  register_sys (@"DIVIDE", divide);
  register_sys (@"ADD-FIXNUMS", add_fixnums);
  register_sys (@"SUBTRACT-FIXNUMS", subtract_fixnums);
  register_sys (@"MULTIPLY-FIXNUMS", multiply_fixnums);
  register_sys (@"IDIVIDE-FIXNUMS", idivide_fixnums);
  register_sys (@"LIST", (id (*)())list);
  register_sys (@"MACROEXPAND-1", (id (*)())macroexpand_1);
  register_sys (@"SHADOW", (id (*)())shadow_);
  register_sys (@"EXPORT", (id (*)())export);
  register_sys (@"UNEXPORT", (id (*)())unexport);
  register_sys (@"FIND-PACKAGE", find_package);
  register_sys (@"STRING", string);
  register_sys (@"GENSYM", (id (*)())gensym);
  register_sys (@"MAKE-SYMBOL", make_symbol);
  register_sys (@"INTERN", (id (*)())intern);
  register_sys (@"IMPORT", (id (*)())import);
  register_sys (@"OBJC-CLASS-OF", objc_class_of);
  register_sys (@"OBJC-SUBCLASSP", objc_subclassp);
  register_sys (@"FIND-OBJC-CLASS", find_objc_class);
  register_sys (@"NS-LOG", ns_log);
  register_sys (@"SYMBOL-NAME", symbol_name);
  register_sys (@"PRIMITIVE-TYPE-OF", primitive_type_of);
  register_sys (@"SEND-BY-NAME", (id (*)())send_by_name);
  register_sys (@"DECLARATIONS-AND-DOC-AND-FORMS", declarations_and_doc_and_forms);
  register_sys (@"DECLARATIONS-AND-FORMS", declarations_and_forms);
  register_sys (@"COMPILE", compile);
  register_sys (@"%FSET", fset);
  register_sys (@"SET", set);
  register_sys (@"%MACROSET", macroset);
  register_sys (@"APPLY", apply);
  register_sys (@"EVAL", eval);
}

+(void) registerBuiltins
{
  // Do the real work in +initialize.
}
@end
