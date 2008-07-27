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
static MLKSymbol *IF;
static MLKSymbol *IN_PACKAGE;
static MLKSymbol *DECLARE;
static MLKSymbol *PROGN;
static MLKSymbol *TAGBODY;
static MLKSymbol *GO;
static MLKSymbol *CATCH;
static MLKSymbol *THROW;
static MLKSymbol *LAMBDA;
static MLKSymbol *LET;
static MLKSymbol *APPLY;
static MLKSymbol *FUNCALL;
static MLKSymbol *EVAL;
static MLKSymbol *QUOTE;
static MLKSymbol *SETQ;
static MLKSymbol *SETF;
static MLKSymbol *SET;
static MLKSymbol *_FSET;
static MLKSymbol *PROGV;
static MLKSymbol *UNWIND_PROTECT;
static MLKSymbol *VALUES;
static MLKSymbol *_DEFMACRO;
static MLKSymbol *_LAMBDA;


@implementation MLKInterpreter
+(void) initialize
{
  cl = [MLKPackage findPackage:@"COMMON-LISP"];
  sys = [MLKPackage findPackage:@"TOILET-SYSTEM"];

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
  APPLY = [cl intern:@"APPLY"];
  EVAL = [cl intern:@"EVAL"];
  QUOTE = [cl intern:@"QUOTE"];
  SETQ = [cl intern:@"SETQ"];
  SETF = [cl intern:@"SETF"];
  SET = [cl intern:@"SET"];
  _FSET = [sys intern:@"%FSET"];
  PROGV = [cl intern:@"PROGV"];
  VALUES = [cl intern:@"VALUES"];
  UNWIND_PROTECT = [cl intern:@"UNWIND-PROTECT"];
  _DEFMACRO = [sys intern:@"%DEFMACRO"];
  _LAMBDA = [sys intern:@"%LAMBDA"];
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


#define RETURN_VALUE(thing)                     \
  { return [NSArray arrayWithObject:nullify(thing)]; }


+(NSArray*) eval:(id)program
            inLexicalContext:(MLKLexicalContext *)context
            withEnvironment:(MLKLexicalEnvironment *)lexenv
            expandOnly:(BOOL)expandOnly
{
  MLKDynamicContext *dynamicContext = [MLKDynamicContext currentContext];

  //  NSLog (@"eval: %@", [program descriptionForLisp]);

  if (!program || [program isKindOfClass:[MLKSymbol class]])
    {
      //NSLog (@"Processing symbol.");
      if ([context symbolNamesSymbolMacro:program])
        {
          id macrofun = [context macroForSymbol:program];
          id expansion = [macrofun applyToArray:
                                     [NSArray arrayWithObjects:
                                                program, context, nil]];
          return [self eval:expansion
                       inLexicalContext:context
                       withEnvironment:lexenv
                       expandOnly:expandOnly];
        }
      else if ([context variableIsLexical:program])
        {
          //NSLog (@"Processing lexical variable %@.", [program descriptionForLisp]);
          //NSLog (@"Lexical environment: %@.", lexenv);
          //NSLog (@"Lexical variable value: %@.", [lexenv valueForSymbol:program]);
          if (expandOnly)
            RETURN_VALUE (program);

          RETURN_VALUE ([lexenv valueForSymbol:program]);
        }
      else
        {
          //NSLog (@"Processing special variable %@.", [program descriptionForLisp]);
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

              NS_DURING
                {
                  catchTag = [[self eval:[[program cdr] car]
                                    inLexicalContext:context
                                    withEnvironment:lexenv
                                    expandOnly:expandOnly]
                               objectAtIndex:0];
                  
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
                                               [MLKCons cons:CATCH
                                                        with:[MLKCons cons:catchTag
                                                                      with:values]]],
                                    NSArray *);

                  [MLKDynamicContext popContext];
                  RELEASE (newctx);

                  NS_VALUERETURN (values, NSArray *);
                }
              NS_HANDLER
                {
                  [MLKDynamicContext popContext];
                  RELEASE (newctx);

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
              NSArray *evaluand = denullify([[self eval:[program cdr]
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

              closure = AUTORELEASE ([[MLKInterpretedClosure alloc]
                                       initWithBodyForms:body
                                       lambdaListName:lambdaList
                                       context:context
                                       environment:lexenv]);
              return [NSArray arrayWithObject:nullify(closure)];
            }
          else if (car == LET)
            {
              id declarations;
              id clauses;
              id body;
              NSArray *result;
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

              if (expandOnly)
                {
                  id body_expansion = denullify([[self eval:[MLKCons cons:PROGN
                                                                     with:body]
                                                       inLexicalContext:context
                                                       withEnvironment:lexenv
                                                       expandOnly:expandOnly]
                                                  objectAtIndex:0]);
                  RETURN_VALUE ([MLKCons
                                  cons:LET
                                  with:[MLKCons
                                         cons:[[program cdr] car]
                                         with:[MLKCons
                                                cons:declarations
                                                with:[MLKCons cons:body_expansion
                                                              with:nil]]]]);
                }

              env = AUTORELEASE ([[MLKLexicalEnvironment alloc]
                                   initWithParent:lexenv
                                   variables:nil
                                   functions:nil]);

              ctx = AUTORELEASE ([[MLKLexicalContext alloc]
                                   initWithParent:context
                                   variables:nil
                                   functions:nil
                                   goTags:nil
                                   macros:nil
                                   compilerMacros:nil
                                   symbolMacros:nil
                                   declarations:declarations]);

              dynctx = [[MLKDynamicContext alloc]
                         initWithParent:dynamicContext
                         variables:nil
                         handlers:nil
                         restarts:nil
                         catchTags:nil
                         activeHandlerEnvironment:nil];

              clauses = [[program cdr] car];
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
                                               withEnvironment:lexenv]
                                          objectAtIndex:0]);
                    }

                  [ctx addVariable:variable];
                  if ([ctx variableIsLexical:variable])
                    {
                      [env addValue:value forSymbol:variable];
                    }
                  else
                    {
                      [dynctx addValue:value forSymbol:variable];
                    }

                  clauses = [clauses cdr];
                }

              [dynctx pushContext];

              NS_DURING
                {
                  result = [self eval:[MLKCons cons:PROGN with:body]
                                 inLexicalContext:ctx
                                 withEnvironment:env];
                }
              NS_HANDLER
                {
                  [MLKDynamicContext popContext];
                  [localException raise];
                }
              NS_ENDHANDLER;

              [MLKDynamicContext popContext];
              RELEASE (dynctx);

              return result;
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
                                 expandOnly:expandOnly];
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
          else if (car == QUOTE)
            {
              if (expandOnly)
                RETURN_VALUE (program);
              RETURN_VALUE ([[program cdr] car]);
            }
          else if (car == SETQ)
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

              if ([context symbolNamesSymbolMacro:symbol])
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
                  RETURN_VALUE ([MLKCons
                                  cons:SETQ
                                  with:[MLKCons
                                         cons:symbol
                                         with:[MLKCons
                                                cons:value
                                                with:denullify([[self eval:
                                                                        [MLKCons cons:SETQ
                                                                                 with:rest]
                                                                      inLexicalContext:context
                                                                      withEnvironment:lexenv
                                                                      expandOnly:expandOnly]
                                                                 objectAtIndex:0])]]]);
                }

              if ([context variableIsLexical:symbol])
                [lexenv setValue:value forSymbol:symbol];
              else if ([dynamicContext bindingForSymbol:symbol])
                [dynamicContext setValue:value forSymbol:symbol];
              else
                // FIXME: Maybe print a warning.
                [[MLKDynamicContext globalContext] addValue:value
                                                   forSymbol:symbol];


              if (rest)
                return [self eval:[MLKCons cons:SETQ with:rest]
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
          else if (car == TAGBODY)
            {
              //FIXME: ...
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
                                               [catchTag descriptionForLisp],
                                               [values descriptionForLisp]]
                              userInfo:userInfo] raise];
              else
                // FIXME: This should really be a condition rather than
                // an exception.  See CLHS THROW.
                [[NSException exceptionWithName:@"MLKControlError"
                              reason:[NSString stringWithFormat:
                                                 @"THROW without a corresponding CATCH: tag %@, values %@.",
                                               [catchTag descriptionForLisp],
                                               [values descriptionForLisp]]
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
          else if (car == VALUES)
            {
              id results = [NSMutableArray array];
              id rest = program;

              while ((rest = [rest cdr]))
                {
                  [results addObject:
                             [[self eval:[rest car]
                                    inLexicalContext:context
                                    withEnvironment:lexenv
                                    expandOnly:expandOnly]
                               objectAtIndex:0]];
                }

              if (expandOnly)
                RETURN_VALUE ([MLKCons cons:VALUES
                                       with:[MLKCons listWithArray:results]]);
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
                               expandOnly:expandOnly];
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
                                          [car descriptionForLisp]];
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
                       [car descriptionForLisp]];
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
      id code = [MLKReader readFromStream:stream
                           eofError:NO
                           eofValue:eofValue
                           recursive:NO
                           preserveWhitespace:NO];
      //NSLog (@"; LOAD: Reading finished.");
      NSString *formdesc;

      //NSLog (@"%@", code);
      //NSLog (@"%@", [code descriptionForLisp]);
      //NSLog (@"%@", stream);
      //NSLog (@"...");

      if (code == eofValue)
        break;

      if ([code isKindOfClass:[MLKCons class]] && [code cdr])
        formdesc = [NSString stringWithFormat:@"(%@ %@ ...)",
                               [[code car] descriptionForLisp],
                               [[[code cdr] car] descriptionForLisp]];
      else
        formdesc = [code descriptionForLisp];

      fprintf (stderr, "; COMPILE-MINIMALLY: %s\n", [formdesc UTF8String]);
      expansion = denullify([[MLKInterpreter
                               eval:code
                               inLexicalContext:[MLKLexicalContext
                                                  globalContext]
                               withEnvironment:[MLKLexicalEnvironment
                                                 globalEnvironment]
                               expandOnly:YES]
                              objectAtIndex:0]);

      if ([code isKindOfClass:[MLKCons class]] && [code cdr])
        formdesc = [NSString stringWithFormat:@"(%@ %@ ...)",
                               [[expansion car] descriptionForLisp],
                               [[[expansion cdr] car] descriptionForLisp]];
      else
        formdesc = [expansion descriptionForLisp];

      fprintf (stderr, "; LOAD: %s\n", [formdesc UTF8String]);
      result = [MLKInterpreter
                 eval:expansion
                 inLexicalContext:[MLKLexicalContext globalContext]
                 withEnvironment:[MLKLexicalEnvironment globalEnvironment]
                 expandOnly:NO];
      //NSLog (@"; LOAD: Top-level form evaluated.");

      if (print)
        {
          //FIXME
          //NSLog (@"; LOAD: Fnord.  Primary value: %@",
          //       [[result objectAtIndex:0] descriptionForLisp]);
        }
    }

  //NSLog (@"; LOAD: END");
  return YES;
}
@end
