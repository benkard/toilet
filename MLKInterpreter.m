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

#import "MLKInterpretedClosure.h"
#import "MLKCons.h"
#import "MLKDynamicContext.h"
#import "MLKEnvironment.h"
#import "MLKFuncallable.h"
#import "MLKInterpreter.h"
#import "MLKLexicalContext.h"
#import "MLKLexicalEnvironment.h"
#import "MLKPackage.h"
#import "MLKReader.h"
#import "MLKRoot.h"
#import "MLKSymbol.h"
#import "NSObject-MLKPrinting.h"
#import "runtime-compatibility.h"
#import "util.h"

#import <Foundation/NSArray.h>
#import <Foundation/NSException.h>
#import <Foundation/NSNull.h>
#import <Foundation/NSString.h>

#include <stdio.h>


static MLKPackage *cl;
static MLKPackage *sys;
static MLKPackage *keyword;
static MLKSymbol *IF;
static MLKSymbol *IN_PACKAGE;
static MLKSymbol *DECLARE;
static MLKSymbol *PROGN;
static MLKSymbol *TAGBODY;
static MLKSymbol *GO;
static MLKSymbol *CATCH;
static MLKSymbol *THROW;
static MLKSymbol *_FLET;
static MLKSymbol *_MACROLET;
static MLKSymbol *LAMBDA;
static MLKSymbol *LET;
static MLKSymbol *LOCALLY;
static MLKSymbol *APPLY;
static MLKSymbol *FUNCALL;
static MLKSymbol *FUNCTION;
static MLKSymbol *EVAL;
static MLKSymbol *EVAL_WHEN;
static MLKSymbol *QUOTE;
static MLKSymbol *SETQ;
static MLKSymbol *SETF;
static MLKSymbol *SET;
static MLKSymbol *_FSETQ;
static MLKSymbol *_FSET;
static MLKSymbol *SYMBOL_MACROLET;
static MLKSymbol *PROGV;
static MLKSymbol *UNWIND_PROTECT;
static MLKSymbol *VALUES;
static MLKSymbol *_DEFMACRO;
static MLKSymbol *_LAMBDA;
static MLKSymbol *_LOOP;
static MLKSymbol *V_INITP;
static MLKSymbol *COMPILE_TOPLEVEL;
static MLKSymbol *COMPILE;
static MLKSymbol *LOAD_TOPLEVEL;
static MLKSymbol *LOAD;
static MLKSymbol *EXECUTE;
static MLKSymbol *MULTIPLE_VALUE_CALL;


@implementation MLKInterpreter
+(void) initialize
{
  cl = [MLKPackage findPackage:@"COMMON-LISP"];
  sys = [MLKPackage findPackage:@"TOILET-SYSTEM"];
  keyword = [MLKPackage findPackage:@"KEYWORD"];

  IF = [cl intern:@"IF"];
  IN_PACKAGE = [cl intern:@"IN-PACKAGE"];
  DECLARE = [cl intern:@"DECLARE"];
  PROGN = [cl intern:@"PROGN"];
  TAGBODY = [cl intern:@"TAGBODY"];
  GO = [cl intern:@"GO"];
  CATCH = [cl intern:@"CATCH"];
  THROW = [cl intern:@"THROW"];
  LAMBDA = [cl intern:@"LAMBDA"];
  LET = [cl intern:@"LET"];
  LOCALLY = [cl intern:@"LOCALLY"];
  _FLET = [sys intern:@"%FLET"];
  _MACROLET = [sys intern:@"%MACROLET"];
  _LOOP = [sys intern:@"%LOOP"];
  APPLY = [cl intern:@"APPLY"];
  EVAL = [cl intern:@"EVAL"];
  EVAL_WHEN = [cl intern:@"EVAL-WHEN"];
  FUNCALL = [cl intern:@"FUNCALL"];
  FUNCTION = [cl intern:@"FUNCTION"];
  QUOTE = [cl intern:@"QUOTE"];
  SETQ = [cl intern:@"SETQ"];
  SETF = [cl intern:@"SETF"];
  SET = [cl intern:@"SET"];
  _FSETQ = [sys intern:@"%FSETQ"];
  _FSET = [sys intern:@"%FSET"];
  SYMBOL_MACROLET = [cl intern:@"SYMBOL-MACROLET"];
  PROGV = [cl intern:@"PROGV"];
  VALUES = [cl intern:@"VALUES"];
  UNWIND_PROTECT = [cl intern:@"UNWIND-PROTECT"];
  _DEFMACRO = [sys intern:@"%DEFMACRO"];
  _LAMBDA = [sys intern:@"%LAMBDA"];
  V_INITP = [sys intern:@"*SYSTEM-INITIALISED-P*"];
  MULTIPLE_VALUE_CALL = [cl intern:@"MULTIPLE-VALUE-CALL"];

  COMPILE_TOPLEVEL = [keyword intern:@"COMPILE-TOPLEVEL"];
  COMPILE = [cl intern:@"COMPILE"];
  LOAD_TOPLEVEL = [keyword intern:@"LOAD-TOPLEVEL"];
  LOAD = [cl intern:@"LOAD"];
  EXECUTE = [keyword intern:@"EXECUTE"];
}


+(NSArray*) eval:(id)program
            inLexicalContext:(MLKLexicalContext *)context
            withEnvironment:(MLKLexicalEnvironment *)lexenv
{
  return (NSArray *)[self eval:program
                          inLexicalContext:context
                          withEnvironment:lexenv
                          expandOnly:NO];
}


+(NSArray*) eval:(id)program
            inLexicalContext:(MLKLexicalContext *)context
            withEnvironment:(MLKLexicalEnvironment *)lexenv
            expandOnly:(BOOL)expandOnly
{
  return [self eval:program
               inLexicalContext:context
               withEnvironment:lexenv
               mode:(expandOnly ? expand_mode : eval_mode)];
}


#define RETURN_VALUE(thing)                     \
  { return [NSArray arrayWithObject:nullify(thing)]; }


+(NSArray*) eval:(id)program
            inLexicalContext:(MLKLexicalContext *)context
            withEnvironment:(MLKLexicalEnvironment *)lexenv
            mode:(enum MLKProcessingMode)mode
{
  MLKDynamicContext *dynamicContext = [MLKDynamicContext currentContext];
  BOOL expandOnly = (mode != eval_mode);

#define TRACE_EVAL 0
#if TRACE_EVAL
  BOOL trace = NO;

  if ([dynamicContext valueForSymbol:V_INITP])
    trace = YES;

  if (trace)
    NSLog (@"; EVAL: %@", MLKPrintToString(program));
#endif  // TRACE_EVAL

  if (MLKFixnumP (program))
    {
      // Fixnums evaluate to themselves.
      //
      // We need to get this case out of the way as early as possible,
      // as we're going to repeatedly send messages to `program' after
      // this point.
      RETURN_VALUE (program);
    }
  else if (!program || [program isKindOfClass:[MLKSymbol class]])
    {
      if (mode == compile_time_too_mode)
        {
          if (![context symbolNamesSymbolMacro:program])
            {
              return [self eval:program
                           inLexicalContext:context
                           withEnvironment:lexenv
                           mode:expand_mode];
            }
        }

      //NSLog (@"Processing symbol.");
      if ([context symbolNamesSymbolMacro:program])
        {
          id macrofun, expansion;

          macrofun = [context macroForSymbol:program];
          expansion = [macrofun applyToArray:
                                  [NSArray arrayWithObjects:
                                             program, context, nil]];

          return [self eval:expansion
                       inLexicalContext:context
                       withEnvironment:lexenv
                       mode:mode];
        }
      else if ([context variableIsLexical:program])
        {
          //NSLog (@"Processing lexical variable %@.", MLKPrintToString(program));
          //NSLog (@"Lexical environment: %@.", lexenv);
          //NSLog (@"Lexical variable value: %@.", [lexenv valueForSymbol:program]);
          if (expandOnly)
            RETURN_VALUE (program);

          RETURN_VALUE ([lexenv valueForSymbol:program]);
        }
      else
        {
          //NSLog (@"Processing special variable %@.", MLKPrintToString(program));
          //NSLog (@"Dynamic context: %@.", dynamicContext);
          //NSLog (@"Special variable value: %@.", [dynamicContext valueForSymbol:program]);
          if (expandOnly)
            RETURN_VALUE (program);

          RETURN_VALUE ([dynamicContext valueForSymbol:program]);
        }
    }
  else if (![program isKindOfClass:[MLKCons class]])
    {
      // Everything that is not a list or a symbol evaluates to itself.
      RETURN_VALUE (program);
    }
  else
    {
      id car = [program car];

      if ([car isKindOfClass:[MLKSymbol class]] || !car)
        {
          if (mode == compile_time_too_mode)
            {
              if (!([context symbolNamesMacro:program]
                    || car == _MACROLET || car == LOCALLY
                    || car == SYMBOL_MACROLET || car == PROGN))
                {
                  return [self eval:program
                               inLexicalContext:context
                               withEnvironment:lexenv
                               mode:expand_mode];
                }
            }

          if (car == APPLY)
            {
              MLKCons *rest = denullify([[self eval:[[[program cdr] cdr] car]
                                               inLexicalContext:context
                                               withEnvironment:lexenv
                                               expandOnly:expandOnly]
                                          objectAtIndex:0]);

              id function = denullify([[self eval:[[program cdr] car]
                                             inLexicalContext:context
                                             withEnvironment:lexenv
                                             expandOnly:expandOnly]
                                        objectAtIndex:0]);

              if (expandOnly)
                RETURN_VALUE ([MLKCons cons:APPLY
                                       with:[MLKCons cons:function
                                                     with:[MLKCons cons:rest
                                                                   with:nil]]]);

              if ([function isKindOfClass:[MLKSymbol class]])
                function = [lexenv functionForSymbol:function];

              return [function applyToArray:(rest
                                             ? (id)[rest array]
                                             : (id)[NSArray array])];
            }
          else if (car == CATCH)
            {
              id catchTag;
              NSArray *values;
              MLKDynamicContext *newctx;

              catchTag = [[self eval:[[program cdr] car]
                                inLexicalContext:context
                                withEnvironment:lexenv
                                expandOnly:expandOnly]
                           objectAtIndex:0];
                  
              NS_DURING
                {
                  if (!expandOnly)
                    {
                      newctx = [[MLKDynamicContext alloc]
                                 initWithParent:dynamicContext
                                 variables:nil
                                 handlers:nil
                                 restarts:nil
                                 catchTags:[NSSet setWithObject:catchTag]
                                 activeHandlerEnvironment:nil];
                      [newctx pushContext];
                    }

                  values = [self eval:[MLKCons cons:PROGN with:[[program cdr] cdr]]
                                 inLexicalContext:context
                                 withEnvironment:lexenv
                                 expandOnly:expandOnly];

                  if (expandOnly)
                    NS_VALUERETURN ([NSArray arrayWithObject:
                                               [MLKCons
                                                 cons:CATCH
                                                 with:[MLKCons
                                                        cons:catchTag
                                                        with:[[values
                                                                objectAtIndex:0]
                                                               cdr]]]],
                                    NSArray *);

                  [MLKDynamicContext popContext];
                  LRELEASE (newctx);

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

              return nil;
            }
          else if (car == _DEFMACRO)
            {
              // No real lambda lists here.  This SYS::%DEFMACRO is
              // really as low-level as it gets.
              id name = [[program cdr] car];
              id lambdaListAndBody = [[program cdr] cdr];

              id <MLKFuncallable> function;

              if (expandOnly)
                {
                  id lambdaList = [lambdaListAndBody car];
                  id body = [lambdaListAndBody cdr];
                  id body_expansion =
                    denullify([[self eval:[MLKCons cons:PROGN with:body]
                                     inLexicalContext:context
                                     withEnvironment:lexenv
                                     expandOnly:expandOnly]
                                objectAtIndex:0]);
                  RETURN_VALUE ([MLKCons
                                  cons:_DEFMACRO
                                  with:[MLKCons
                                         cons:name
                                         with:[MLKCons
                                                cons:lambdaList
                                                with:[MLKCons
                                                       cons:body_expansion
                                                       with:nil]]]]);
                }

              function = denullify([[self eval:[MLKCons cons:_LAMBDA with:lambdaListAndBody]
                                          inLexicalContext:context
                                          withEnvironment:lexenv
                                          expandOnly:expandOnly]
                                     objectAtIndex:0]);

              [context addMacro:function forSymbol:name];

              RETURN_VALUE (name);
            }
          else if (car == EVAL)
            {
              NSArray *evaluand = denullify([[self eval:[[program cdr] car]
                                                   inLexicalContext:context
                                                   withEnvironment:lexenv
                                                   expandOnly:expandOnly]
                                              objectAtIndex:0]);

              if (expandOnly)
                RETURN_VALUE ([MLKCons cons:EVAL with:[MLKCons cons:evaluand with:nil]]);

              return [self eval:evaluand
                           inLexicalContext:[MLKLexicalContext globalContext]
                           withEnvironment:[MLKLexicalEnvironment
                                             globalEnvironment]];
            }
          else if (car == EVAL_WHEN)
            {
              id situationList = [[program cdr] car];
              id body = [[program cdr] cdr];
              NSArray *situations;
              BOOL ct, lt, e;

              if (!situationList)
                RETURN_VALUE (nil);

              situations = [situationList array];
              ct = ([situations containsObject:COMPILE_TOPLEVEL]
                    || [situations containsObject:COMPILE]);
              lt = ([situations containsObject:LOAD_TOPLEVEL]
                    || [situations containsObject:LOAD]);
              e = ([situations containsObject:EXECUTE]
                   || [situations containsObject:EVAL]);

              switch (mode)
                {
                case eval_mode:
                case expand_mode:
                  if (e)
                    return [self eval:[MLKCons cons:PROGN with:body]
                                 inLexicalContext:context
                                 withEnvironment:lexenv
                                 mode:mode];
                  else
                    RETURN_VALUE (nil);

                case compile_time_too_mode:
                case not_compile_time_mode:
                  if ((ct && lt)
                      || (lt && e && (mode == compile_time_too_mode)))
                    {
                      return [self eval:[MLKCons cons:PROGN with:body]
                                   inLexicalContext:context
                                   withEnvironment:lexenv
                                   mode:compile_time_too_mode];
                    }
                  else if (lt)
                    {
                      return [self eval:[MLKCons cons:PROGN with:body]
                                   inLexicalContext:context
                                   withEnvironment:lexenv
                                   mode:not_compile_time_mode];
                    }
                  else if (ct || (e && mode == compile_time_too_mode))
                    {
                      [self eval:[MLKCons cons:PROGN with:body]
                            inLexicalContext:context
                            withEnvironment:lexenv];
                      RETURN_VALUE (nil);
                    }
                  else
                    {
                      RETURN_VALUE (nil);
                    }
                }
            }
          else if (car == FUNCTION)
            {
              id functionName = [[program cdr] car];

              if ([functionName isKindOfClass:[MLKCons class]]
                  && ([functionName car] == LAMBDA
                      || [functionName car] == _LAMBDA))
                {
                  return [self eval:functionName
                               inLexicalContext:context
                               withEnvironment:lexenv
                               expandOnly:expandOnly];
                }
              else if (expandOnly)
                {
                  RETURN_VALUE (program);
                }
              else
                {
                  // FIXME: Function names need not be symbols.
                  id <MLKFuncallable> function =
                    [lexenv functionForSymbol:functionName];
                  RETURN_VALUE (function);
                }
            }
          else if (car == IF)
            {
              id condition = [[program cdr] car];
              id consequent = [[[program cdr] cdr] car];
              // Incidentally works for the two-clause case:
              id alternative = [[[[program cdr] cdr] cdr] car];

              id condition_value = denullify([[self eval:condition
                                                    inLexicalContext:context
                                                    withEnvironment:lexenv
                                                    expandOnly:expandOnly]
                                               objectAtIndex:0]);

              if (expandOnly)
                {
                  id conseq_expansion = denullify([[self eval:consequent
                                                         inLexicalContext:context
                                                         withEnvironment:lexenv
                                                         expandOnly:expandOnly]
                                                    objectAtIndex:0]);
                  id alt_expansion = denullify([[self eval:alternative
                                                      inLexicalContext:context
                                                      withEnvironment:lexenv
                                                      expandOnly:expandOnly]
                                                 objectAtIndex:0]);
                  RETURN_VALUE ([MLKCons
                                  cons:IF
                                  with:[MLKCons
                                         cons:condition_value
                                         with:[MLKCons
                                                cons:conseq_expansion
                                                with:[MLKCons cons:alt_expansion
                                                              with:nil]]]]);
                }

              if (!condition_value)
                return [self eval:alternative
                             inLexicalContext:context
                             withEnvironment:lexenv
                             expandOnly:expandOnly];
              else
                return [self eval:consequent
                             inLexicalContext:context
                             withEnvironment:lexenv
                             expandOnly:expandOnly];                
            }
          else if (car == IN_PACKAGE)
            {
              if (expandOnly)
                RETURN_VALUE (program);

              id cadr = [[program cdr] car];
              id package = [MLKPackage findPackage:stringify(cadr)];

              [[MLKDynamicContext currentContext]
                setValue:package
                forSymbol:[[MLKPackage findPackage:@"COMMON-LISP"]
                            intern:@"*PACKAGE*"]];

              RETURN_VALUE (package);
            }
          else if (car == _LAMBDA)
            {
              // A bare-bones LAMBDA without a real lambda list.  What
              // would be a lambda list in a real LAMBDA form must be a
              // symbol here.
              id lambdaList = [[program cdr] car];
              id body = [[program cdr] cdr];
              MLKInterpretedClosure *closure;

              if (expandOnly)
                {
                  id body_expansion = denullify([[self eval:[MLKCons cons:PROGN
                                                                     with:body]
                                                       inLexicalContext:context
                                                       withEnvironment:lexenv
                                                       expandOnly:expandOnly]
                                                  objectAtIndex:0]);
                  RETURN_VALUE ([MLKCons
                                  cons:_LAMBDA
                                  with:[MLKCons cons:lambdaList
                                                with:[MLKCons cons:body_expansion
                                                              with:nil]]]);
                }

              closure = LAUTORELEASE ([[MLKInterpretedClosure alloc]
                                       initWithBodyForms:body
                                       lambdaListName:lambdaList
                                       context:context
                                       environment:lexenv]);
              return [NSArray arrayWithObject:nullify(closure)];
            }
          else if (car == _MACROLET)
            {
              id declarations;
              id clauses;
              id body;
              NSArray *result;
              MLKLexicalContext *ctx;

              body = [[program cdr] cdr];

              if ([[body car] isKindOfClass:[MLKCons class]]
                  && [[body car] car] == DECLARE)
                {
                  declarations = [[body car] cdr];
                  body = [body cdr];
                }
              else
                {
                  declarations = nil;
                }

              ctx = LAUTORELEASE ([[MLKLexicalContext alloc]
                                   initWithParent:context
                                   variables:nil
                                   functions:nil
                                   goTags:nil
                                   macros:nil
                                   compilerMacros:nil
                                   symbolMacros:nil
                                   declarations:declarations]);

              clauses = [[program cdr] car];
              while (clauses)
                {
                  id clause = [clauses car];
                  id name, value;

                  name = [clause car];
                  value = denullify([[self eval:[MLKCons cons:_LAMBDA
                                                         with:[clause cdr]]
                                           inLexicalContext:context
                                           withEnvironment:lexenv
                                           expandOnly:NO]  //!
                                      objectAtIndex:0]);

                  [ctx addMacro:value forSymbol:name];

                  clauses = [clauses cdr];
                }

              result = [self eval:[MLKCons cons:PROGN with:body]
                             inLexicalContext:ctx
                             withEnvironment:lexenv
                             mode:mode];

              if (expandOnly)
                {
                  RETURN_VALUE ([MLKCons
                                  cons:LET
                                  with:[MLKCons
                                         cons:nil
                                         with:[MLKCons
                                                cons:[MLKCons cons:DECLARE
                                                              with:declarations]
                                                with:[[result objectAtIndex:0] cdr]]]]);
                }
              else
                {
                  return result;
                }
            }
          else if (car == _FLET)
            {
              id declarations;
              id clauses;
              NSMutableArray *new_clauses;
              id body;
              NSArray *result;
              MLKLexicalContext *ctx;
              MLKLexicalEnvironment *env;

              body = [[program cdr] cdr];

              if ([[body car] isKindOfClass:[MLKCons class]]
                  && [[body car] car] == DECLARE)
                {
                  declarations = [[body car] cdr];
                  body = [body cdr];
                }
              else
                {
                  declarations = nil;
                }

              ctx = LAUTORELEASE ([[MLKLexicalContext alloc]
                                   initWithParent:context
                                   variables:nil
                                   functions:nil
                                   goTags:nil
                                   macros:nil
                                   compilerMacros:nil
                                   symbolMacros:nil
                                   declarations:declarations]);

              if (!expandOnly)
                env = LAUTORELEASE ([[MLKLexicalEnvironment alloc]
                                     initWithParent:lexenv
                                     variables:nil
                                     functions:nil]);

              clauses = [[program cdr] car];
              new_clauses = [NSMutableArray array];
              while (clauses)
                {
                  id clause = [clauses car];
                  id name, value;

                  name = [clause car];

                  value = denullify([[self eval:[MLKCons cons:_LAMBDA
                                                         with:[clause cdr]]
                                           inLexicalContext:context
                                           withEnvironment:lexenv
                                           expandOnly:expandOnly]
                                      objectAtIndex:0]);

                  [ctx addFunction:name];

                  if (!expandOnly)
                    [env addFunction:value forSymbol:name];
                  else
                    [new_clauses addObject:[MLKCons cons:name with:[value cdr]]];

                  clauses = [clauses cdr];
                }

              result = [self eval:[MLKCons cons:PROGN with:body]
                             inLexicalContext:ctx
                             withEnvironment:(expandOnly ? lexenv : env)
                             expandOnly:expandOnly];

              if (expandOnly)
                {
                  RETURN_VALUE ([MLKCons
                                  cons:_FLET
                                  with:[MLKCons
                                         cons:[MLKCons listWithArray:new_clauses]
                                         with:[MLKCons
                                                cons:[MLKCons cons:DECLARE
                                                              with:declarations]
                                                with:[[result objectAtIndex:0] cdr]]]]);
                }
              else
                {
                  return result;
                }
            }
          else if (car == LET)
            {
              id declarations;
              id clauses;
              id body;
              NSArray *result;
              NSMutableArray *new_clauses;
              MLKLexicalContext *ctx;
              MLKLexicalEnvironment *env;
              MLKDynamicContext *dynctx;

              body = [[program cdr] cdr];
              if ([[body car] isKindOfClass:[MLKCons class]]
                  && [[body car] car] == DECLARE)
                {
                  declarations = [[body car] cdr];
                  body = [body cdr];
                }
              else
                {
                  declarations = nil;
                }

              ctx = LAUTORELEASE ([[MLKLexicalContext alloc]
                                   initWithParent:context
                                   variables:nil
                                   functions:nil
                                   goTags:nil
                                   macros:nil
                                   compilerMacros:nil
                                   symbolMacros:nil
                                   declarations:declarations]);

              if (!expandOnly)
                {
                  env = LAUTORELEASE ([[MLKLexicalEnvironment alloc]
                                       initWithParent:lexenv
                                       variables:nil
                                       functions:nil]);

                  dynctx = [[MLKDynamicContext alloc]
                             initWithParent:dynamicContext
                             variables:nil
                             handlers:nil
                             restarts:nil
                             catchTags:nil
                             activeHandlerEnvironment:nil];
                }

              clauses = [[program cdr] car];
              new_clauses = [NSMutableArray array];
              while (clauses)
                {
                  id clause = [clauses car];
                  id variable, value;

                  if (!clause || [clause isKindOfClass:[MLKSymbol class]])
                    {
                      variable = clause;
                      value = nil;
                    }
                  else if ([clause cdr] == nil)
                    {
                      variable = [clause car];
                      value = nil;
                    }
                  else
                    {
                      variable = [clause car];
                      value = denullify([[self eval:[[clause cdr] car]
                                               inLexicalContext:context
                                               withEnvironment:lexenv
                                               expandOnly:expandOnly]
                                          objectAtIndex:0]);
                    }

                  if (expandOnly)
                    {
                      [new_clauses addObject:[MLKCons cons:variable
                                                      with:[MLKCons cons:value
                                                                    with:nil]]];
                    }
                  else
                    {
                      [ctx addVariable:variable];
                      if ([ctx variableIsLexical:variable])
                        {
                          [env addValue:value forSymbol:variable];
                        }
                      else
                        {
                          [dynctx addValue:value forSymbol:variable];
                        }
                    }

                  clauses = [clauses cdr];
                }

              if (expandOnly)
                {
                  result = [self eval:[MLKCons cons:PROGN with:body]
                                 inLexicalContext:ctx
                                 withEnvironment:lexenv
                                 expandOnly:YES];

                  RETURN_VALUE ([MLKCons
                                  cons:LET
                                  with:[MLKCons
                                         cons:[MLKCons listWithArray:new_clauses]
                                         with:[MLKCons
                                                cons:[MLKCons cons:DECLARE
                                                              with:declarations]
                                                with:[[result objectAtIndex:0] cdr]]]]);
                }
              else
                {
                  [dynctx pushContext];

                  NS_DURING
                    {
                      result = [self eval:[MLKCons cons:PROGN with:body]
                                     inLexicalContext:ctx
                                     withEnvironment:env
                                     expandOnly:NO];
                    }
                  NS_HANDLER
                    {
                      [MLKDynamicContext popContext];
                      [localException raise];
                    }
                  NS_ENDHANDLER;

                  [MLKDynamicContext popContext];
                  LRELEASE (dynctx);

                  return result;
                }
            }
          else if (car == _LOOP)
            {
              id rest;

              if (expandOnly)
                {
                  RETURN_VALUE ([MLKCons cons:_LOOP
                                         with:[[[self eval:[MLKCons cons:PROGN
                                                                    with:[program cdr]]
                                                      inLexicalContext:context
                                                      withEnvironment:lexenv
                                                      expandOnly:YES]
                                                 objectAtIndex:0]
                                                cdr]]);
                }

              while (YES)
                {
                  rest = program;
                  while ((rest = [rest cdr]))
                    {
                      [self eval:[rest car]
                            inLexicalContext:context
                            withEnvironment:lexenv
                            expandOnly:expandOnly];
                    }
                }

              RETURN_VALUE (nil);  // never reached
            }
          else if (car == MULTIPLE_VALUE_CALL)
            {
              NSMutableArray *results = [NSMutableArray array];
              id rest = [program cdr];
              id function = [[self eval:[rest car]
                                   inLexicalContext:context
                                   withEnvironment:lexenv
                                   mode:mode]
                              objectAtIndex:0];

              while ((rest = [rest cdr]))
                {
                  id values = [self eval:[rest car]
                                    inLexicalContext:context
                                    withEnvironment:lexenv
                                    mode:mode];
                  [results addObjectsFromArray:values];
                }

              if (expandOnly)
                {
                  RETURN_VALUE ([MLKCons
                                  cons:MULTIPLE_VALUE_CALL
                                  with:[MLKCons
                                         cons:function
                                         with:[MLKCons
                                                listWithArray:results]]]);
                }
              else
                {
                  return [function applyToArray:results];
                }
            }
          else if (car == PROGN)
            {
              id result = nil;
              id rest = program;
              NSMutableArray *results = [NSMutableArray array];
              while ((rest = [rest cdr]))
                {
                  result = [self eval:[rest car]
                                 inLexicalContext:context
                                 withEnvironment:lexenv
                                 mode:mode];
                  if (expandOnly)
                    [results addObjectsFromArray:result];
                }

              if (expandOnly)
                {
                  RETURN_VALUE ([MLKCons cons:PROGN
                                         with:[MLKCons listWithArray:results]]);
                }
              else
                return result;
            }
          else if (car == PROGV)
            {
              id variables, values, body, result;
              MLKDynamicContext *dynctx;

              if (expandOnly)
                {
                  RETURN_VALUE ([MLKCons
                                  cons:PROGV
                                  with:[denullify([[self eval:[MLKCons
                                                                cons:PROGN
                                                                with:[program cdr]]
                                                         inLexicalContext:context
                                                         withEnvironment:lexenv
                                                         mode:mode]
                                                    objectAtIndex:0]) cdr]]);
                }

              dynctx = [[MLKDynamicContext alloc]
                         initWithParent:dynamicContext
                         variables:nil
                         handlers:nil
                         restarts:nil
                         catchTags:nil
                         activeHandlerEnvironment:nil];

              body = [[[program cdr] cdr] cdr];
              variables = denullify ([[self eval:[[program cdr] car]
                                            inLexicalContext:context
                                            withEnvironment:lexenv]
                                       objectAtIndex:0]);
              values = denullify ([[self eval:[[[program cdr] cdr] car]
                                         inLexicalContext:context
                                         withEnvironment:lexenv]
                                    objectAtIndex:0]);

              for (; variables; (variables = [variables cdr], values = [values cdr]))
                {
                  id var = [variables car];
                  id value = [values car];

                  [dynctx addValue:value forSymbol:var];
                }

              [dynctx pushContext];

              NS_DURING
                {
                  result = [self eval:[MLKCons cons:PROGN with:body]
                                 inLexicalContext:context
                                 withEnvironment:lexenv];
                }
              NS_HANDLER
                {
                  [MLKDynamicContext popContext];
                  [localException raise];
                }
              NS_ENDHANDLER;

              [MLKDynamicContext popContext];
              LRELEASE (dynctx);

              return result;
            }
          else if (car == QUOTE)
            {
              if (expandOnly)
                RETURN_VALUE (program);
              RETURN_VALUE ([[program cdr] car]);
            }
          else if (car == SETQ || car == _FSETQ)
            {
              id symbol = [[program cdr] car];
              id value = [[self eval:[[[program cdr] cdr] car]
                                inLexicalContext:context
                                withEnvironment:lexenv
                                expandOnly:expandOnly]
                           objectAtIndex:0];
              id rest = [[[program cdr] cdr] cdr];

              if (![program cdr])
                RETURN_VALUE (nil);

              if (car == SETQ && [context symbolNamesSymbolMacro:symbol])
                {
                  id macrofun = [context symbolMacroForSymbol:symbol];
                  id expansion = [macrofun applyToArray:
                                             [NSArray arrayWithObjects:
                                                        program, context, nil]];
                  return [self eval:
                                 [MLKCons cons:SETF
                                          with:
                                            [MLKCons cons:expansion
                                                     with:
                                                       [[program cdr] cdr]]]
                               inLexicalContext:context
                               withEnvironment:lexenv
                               expandOnly:expandOnly];
                }

              if (expandOnly)
                {
                  id thisSETQ = [MLKCons
                                  cons:car
                                  with:[MLKCons
                                         cons:symbol
                                         with:[MLKCons
                                                cons:value
                                                with:nil]]];
                  id more = denullify([[self eval:[MLKCons cons:car with:rest]
                                             inLexicalContext:context
                                             withEnvironment:lexenv
                                             expandOnly:expandOnly]
                                        objectAtIndex:0]);

                  if (!more)
                    {
                      RETURN_VALUE (thisSETQ);
                    }
                  else
                    {
                      RETURN_VALUE ([MLKCons cons:PROGN
                                             with:[MLKCons
                                                    cons:thisSETQ
                                                    with:[MLKCons
                                                           cons:more
                                                           with:nil]]]);
                      
                    }
                }

              if (car == _FSETQ)
                {
                  if ([context symbolNamesFunction:symbol])
                    {
                      [lexenv setFunction:value forSymbol:symbol];
                    }
                  else
                    {
                      // FIXME: Maybe print a warning.
                      [[MLKLexicalContext globalContext] addFunction:symbol];
                      [[MLKLexicalEnvironment globalEnvironment]
                        addFunction:value
                        forSymbol:symbol];
                    }
                }
              else if ([context variableIsLexical:symbol])
                [lexenv setValue:value forSymbol:symbol];
              else if ([dynamicContext bindingForSymbol:symbol])
                [dynamicContext setValue:value forSymbol:symbol];
              else
                // FIXME: Maybe print a warning.
                [[MLKDynamicContext globalContext] addValue:value
                                                   forSymbol:symbol];


              if (rest)
                return [self eval:[MLKCons cons:car with:rest]
                             inLexicalContext:context
                             withEnvironment:lexenv];
              else
                RETURN_VALUE (value);
            }
          else if (car == SET)
            {
              id symbol = [[self eval:[[program cdr] car]
                                 inLexicalContext:context
                                 withEnvironment:lexenv
                                 expandOnly:expandOnly]
                           objectAtIndex:0];
              id value = [[self eval:[[[program cdr] cdr] car]
                                inLexicalContext:context
                                withEnvironment:lexenv
                                expandOnly:expandOnly]
                          objectAtIndex:0];

              if (expandOnly)
                RETURN_VALUE ([MLKCons cons:SET
                                       with:[MLKCons cons:symbol
                                                     with:[MLKCons cons:value
                                                                   with:nil]]]);

              if ([dynamicContext bindingForSymbol:symbol])
                [dynamicContext setValue:value forSymbol:symbol];
              else
                [[MLKDynamicContext globalContext] addValue:value
                                                   forSymbol:symbol];

              return [NSArray arrayWithObject:symbol];
            }
          else if (car == _FSET)
            {
              // Like SET, but for the function cell.
              id symbol = [[self eval:[[program cdr] car]
                                 inLexicalContext:context
                                 withEnvironment:lexenv
                                 expandOnly:expandOnly]
                            objectAtIndex:0];
              id value = [[self eval:[[[program cdr] cdr] car]
                                inLexicalContext:context
                                withEnvironment:lexenv
                                expandOnly:expandOnly]
                           objectAtIndex:0];

              if (expandOnly)
                RETURN_VALUE ([MLKCons cons:_FSET
                                       with:[MLKCons cons:symbol
                                                     with:[MLKCons cons:value
                                                                   with:nil]]]);

              [[MLKLexicalContext globalContext] addFunction:symbol];
              [[MLKLexicalEnvironment globalEnvironment] addFunction:value
                                                         forSymbol:symbol];

              return [NSArray arrayWithObject:symbol];
            }
          else if (car == THROW)
            {
              id catchTag;
              NSArray *values;
              NSDictionary *userInfo;

              catchTag = [[self eval:[[program cdr] car]
                                inLexicalContext:context
                                withEnvironment:lexenv
                                expandOnly:expandOnly]
                           objectAtIndex:0];

              values = [self eval:[[[program cdr] cdr] car]
                             inLexicalContext:context
                             withEnvironment:lexenv
                             expandOnly:expandOnly];

              if (expandOnly)
                RETURN_VALUE ([MLKCons cons:THROW
                                       with:[MLKCons cons:denullify(catchTag)
                                                     with:[MLKCons cons:denullify([values objectAtIndex:0])
                                                                   with:nil]]]);

              userInfo = [NSDictionary dictionaryWithObjectsAndKeys:
                                         catchTag, @"THROWN TAG",
                                         values, @"THROWN OBJECTS", nil];

              if ([dynamicContext catchTagIsEstablished:denullify (catchTag)])
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
          else if (car == UNWIND_PROTECT)
            {
              NSArray *results;

              if (expandOnly)
                {
                  id protectee = [self eval:[[program cdr] car]
                                       inLexicalContext:context
                                       withEnvironment:lexenv
                                       expandOnly:expandOnly];
                  id protection = [self eval:[MLKCons cons:PROGN
                                                      with:[[program cdr] cdr]]
                                        inLexicalContext:context
                                        withEnvironment:lexenv
                                        expandOnly:expandOnly];
                  RETURN_VALUE ([MLKCons cons:UNWIND_PROTECT
                                         with:[MLKCons cons:protectee
                                                       with:[MLKCons cons:protection
                                                                     with:nil]]]);
                }

              NS_DURING
                {
                  results = [self eval:[[program cdr] car]
                                  inLexicalContext:context
                                  withEnvironment:lexenv];
                }
              NS_HANDLER
                {
                  [self eval:[MLKCons cons:PROGN with:[[program cdr] cdr]]
                        inLexicalContext:context
                        withEnvironment:lexenv
                        expandOnly:expandOnly];

                  [localException raise];
                }
              NS_ENDHANDLER;

              [self eval:[MLKCons cons:PROGN with:[[program cdr] cdr]]
                    inLexicalContext:context
                    withEnvironment:lexenv];

              return results;
            }
          else
            {
              if ([context symbolNamesFunction:car])
                {
                  id function;
                  MLKCons *rest = [program cdr];
                  NSMutableArray *args = [NSMutableArray array];
                  
                  while (rest)
                    {
                      id result = [[self eval:[rest car]
                                         inLexicalContext:context
                                         withEnvironment:lexenv
                                         expandOnly:expandOnly]
                                    objectAtIndex:0];
                      [args addObject:result];
                      rest = [rest cdr];
                    }

                  if (expandOnly)
                    {
                      RETURN_VALUE ([MLKCons cons:[program car]
                                             with:[MLKCons listWithArray:args]]);
                    }
                  else
                    {
                      function = [lexenv functionForSymbol:car];
                      return [function applyToArray:args];
                    }
                }
              else if ([context symbolNamesMacro:car])
                {
                  id macrofun = [context macroForSymbol:car];
                  id expansion = denullify([[macrofun
                                              applyToArray:
                                                [NSArray arrayWithObjects:
                                                           program, context, nil]]
                                             objectAtIndex:0]);

                  return [self eval:expansion
                               inLexicalContext:context
                               withEnvironment:lexenv
                               mode:mode];
                }
              else
                {
                  NSMutableArray *args = [NSMutableArray array];
                  MLKCons *rest = [program cdr];
                  NSArray *results;

                  while (rest)
                    {
                      id result = [[self eval:[rest car]
                                         inLexicalContext:context
                                         withEnvironment:lexenv
                                         expandOnly:expandOnly]
                                    objectAtIndex:0];
                      [args addObject:result];
                      rest = [rest cdr];
                    }

                  if (expandOnly)
                    {
                      RETURN_VALUE ([MLKCons cons:[program car]
                                             with:[MLKCons listWithArray:args]]);
                    }

                  results = [MLKRoot dispatch:car withArguments:args];

                  if (results)
                    {
                      return results;
                    }
                  else
                    {
                      [NSException raise:@"MLKNoSuchOperatorException"
                                   format:@"%@ does not name a known operator.",
                                          MLKPrintToString(car)];
                      return nil;
                    }
                }
            }
        }
      else if ([car isKindOfClass:[MLKCons class]] && [car car] == LAMBDA)
        {
          return [self eval:[MLKCons cons:FUNCALL with:program]
                       inLexicalContext:context
                       withEnvironment:lexenv
                       expandOnly:expandOnly];
        }
      else
        {
          [NSException raise:@"MLKInvalidExpressionException"
                       format:@"%@ is not a valid operator name.",
                       MLKPrintToString(car)];
          return nil;  
        }
    }
}


+(BOOL) load:(MLKStream *)stream verbose:(BOOL)verbose print:(BOOL)print
{
  id eofValue = [[NSObject alloc] init];

  while (YES)
    {
      id result;
      id expansion;
      //NSLog (@"; LOAD: Reding a form.");
      id code;
      //NSLog (@"; LOAD: Reading finished.");
      NSString *formdesc;
      NSAutoreleasePool *pool;

      //NSLog (@"%@", code);
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

      if ([code isKindOfClass:[MLKCons class]] && [code cdr])
        formdesc = [NSString stringWithFormat:@"(%@ %@ ...)",
                               MLKPrintToString([code car]),
                               MLKPrintToString([[code cdr] car])];
      else
        formdesc = MLKPrintToString(code);

      //fprintf (stderr, "; COMPILE-MINIMALLY: %s\n", [formdesc UTF8String]);
      fprintf (stderr, "; LOAD: %s\n", [formdesc UTF8String]);
      expansion = denullify([[MLKInterpreter
                               eval:code
                               inLexicalContext:[MLKLexicalContext
                                                  globalContext]
                               withEnvironment:[MLKLexicalEnvironment
                                                 globalEnvironment]
                               mode:not_compile_time_mode]
                              objectAtIndex:0]);

      if ([code isKindOfClass:[MLKCons class]] && [code cdr])
        formdesc = [NSString stringWithFormat:@"(%@ %@ ...)",
                               MLKPrintToString([expansion car]),
                               MLKPrintToString([[expansion cdr] car])];
      else
        formdesc = MLKPrintToString(expansion);

      //fprintf (stderr, "; LOAD: %s\n", [formdesc UTF8String]);
      result = [MLKInterpreter
                 eval:expansion
                 inLexicalContext:[MLKLexicalContext globalContext]
                 withEnvironment:[MLKLexicalEnvironment globalEnvironment]
                 expandOnly:NO];
      //NSLog (@"; LOAD: Top-level form evaluated.");

      LRELEASE (pool);

      if (print)
        {
          //FIXME
          //NSLog (@"; LOAD: Fnord.  Primary value: %@",
          //       MLKPrintToString([result objectAtIndex:0]));
        }
    }

  //NSLog (@"; LOAD: END");
  return YES;
}
@end
