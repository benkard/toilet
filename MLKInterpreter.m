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
#import "MLKRoot.h"
#import "MLKSymbol.h"
#import "runtime-compatibility.h"

#import <Foundation/NSArray.h>
#import <Foundation/NSException.h>
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


static MLKPackage *cl;
static MLKPackage *sys;
static MLKSymbol *IF;
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
static MLKSymbol *PROGV;
static MLKSymbol *VALUES;
static MLKSymbol *_DEFMACRO;
static MLKSymbol *_LAMBDA;


@implementation MLKInterpreter
+(void) initialize
{
  cl = [MLKPackage findPackage:@"COMMON-LISP"];
  sys = [MLKPackage findPackage:@"TOILET-SYSTEM"];

  IF = [cl intern:@"IF"];
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
  PROGV = [cl intern:@"PROGV"];
  VALUES = [cl intern:@"VALUES"];
  _DEFMACRO = [sys intern:@"%DEFMACRO"];
  _LAMBDA = [sys intern:@"%LAMBDA"];
}


+(NSArray*) eval:(id)program
            inLexicalContext:(MLKLexicalContext *)context
            withEnvironment:(MLKLexicalEnvironment *)lexenv
{
  MLKDynamicContext *dynamicContext = [MLKDynamicContext currentContext];

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
                       withEnvironment:lexenv];
        }
      else if ([context variableIsLexical:program])
        {
          //NSLog (@"Processing lexical variable.");
          return [NSArray arrayWithObject:nullify([lexenv valueForSymbol:program])];
        }
      else
        {
          //NSLog (@"Processing special variable.");
          return [NSArray arrayWithObject:nullify([dynamicContext valueForSymbol:program])];
        }
    }
  else if (![program isKindOfClass:[MLKCons class]])
    {
      // Everything that is not a list or a symbol evaluates to itself.
      return [NSArray arrayWithObject:nullify(program)];
    }
  else
    {
      id car = [program car];

      if ([car isKindOfClass:[MLKSymbol class]])
        {
          if (car == APPLY)
            {
              MLKCons *rest = denullify([[self eval:[[[program cdr] cdr] car]
                                               inLexicalContext:context
                                               withEnvironment:lexenv]
                                          objectAtIndex:0]);

              id <MLKFuncallable> function = denullify([[self eval:[[program cdr] car]
                                                              inLexicalContext:context
                                                              withEnvironment:lexenv]
                                                         objectAtIndex:0]);

              return [function applyToArray:(rest
                                             ? (id)[rest array]
                                             : (id)[NSArray array])];
            }
          else if (car == _DEFMACRO)
            {
              // No real lambda lists here.  This SYS::%DEFMACRO is
              // really as low-level as it gets.
              id name = [[program cdr] car];
              id lambdaListAndBody = [[program cdr] cdr];

              id <MLKFuncallable> function;

              function = denullify([[self eval:[MLKCons cons:_LAMBDA with:lambdaListAndBody]
                                          inLexicalContext:context
                                          withEnvironment:lexenv]
                                     objectAtIndex:0]);

              [context addMacro:function forSymbol:name];

              return [NSArray arrayWithObject:nullify(name)];
            }
          else if (car == EVAL)
            {
              return [self eval:denullify([[self eval:[program cdr]
                                                 inLexicalContext:context
                                                 withEnvironment:lexenv]
                                            objectAtIndex:0])
                           inLexicalContext:[MLKLexicalContext globalContext]
                           withEnvironment:[MLKLexicalEnvironment
                                             globalEnvironment]];
            }
          else if (car == _LAMBDA)
            {
              // A bare-bones LAMBDA without a real lambda list.  What
              // would be a lambda list in a real LAMBDA form must be a
              // symbol here.
              id lambdaList = [[program cdr] car];
              id body = [[program cdr] cdr];
              MLKInterpretedClosure *closure;

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

                  if ([ctx variableIsLexical:variable])
                    {
                      [ctx addVariable:variable];
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
              RELEASE (dynctx);

              return result;
            }
          else if (car == PROGN)
            {
              id result = nil;
              id rest = program;
              while ((rest = [rest cdr]))
                {
                  result = [self eval:[rest car]
                                 inLexicalContext:context
                                 withEnvironment:lexenv];
                }

              return result;
            }
          else if (car == QUOTE)
            {
              return [NSArray arrayWithObject:nullify([[program cdr] car])];
            }
          else if (car == SETQ)
            {
              //FIXME: ...
              //FIXME: Don't forget handling symbol macros correctly.
            }
          else if (car == TAGBODY)
            {
              //FIXME: ...
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
                                    withEnvironment:lexenv]
                               objectAtIndex:0]];
                }

              return results;
            }
          else
            {
              if ([context symbolNamesFunction:car])
                {
                  id function = [lexenv functionForSymbol:car];
                  MLKCons *rest = [program cdr];
                  NSMutableArray *args = [NSMutableArray array];
                  
                  while (rest)
                    {
                      id result = [[self eval:[rest car]
                                         inLexicalContext:context
                                         withEnvironment:lexenv]
                                    objectAtIndex:0];
                      [args addObject:result];
                      rest = [rest cdr];
                    }
                  
                  return [function applyToArray:args];
                }
              else if ([context symbolNamesMacro:car])
                {
                  id macrofun = [context macroForSymbol:car];
                  id expansion = denullify([[macrofun
                                              applyToArray:
                                                [NSArray arrayWithObjects:
                                                           program, context, nil]]
                                             objectAtIndex:0]);
;
                  return [self eval:expansion
                               inLexicalContext:context
                               withEnvironment:lexenv];
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
                                         withEnvironment:lexenv]
                                    objectAtIndex:0];
                      [args addObject:result];
                      rest = [rest cdr];
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
      else if (![car isKindOfClass:[MLKCons class]] && [car car] == LAMBDA)
        {
          return [self eval:[MLKCons cons:FUNCALL with:program]
                       inLexicalContext:context
                       withEnvironment:lexenv];
        }
    }
}
@end
