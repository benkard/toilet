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
#import "MLKDynamicContext.h"
#import "MLKEnvironment.h"
#import "MLKFuncallable.h"
#import "MLKInterpreter.h"
#import "MLKLexicalContext.h"
#import "MLKLexicalEnvironment.h"
#import "MLKPackage.h"
#import "MLKSymbol.h"
#import "runtime-compatibility.h"

#import <Foundation/NSArray.h>
#import <Foundation/NSException.h>
#import <Foundation/NSNull.h>
#import <Foundation/NSString.h>


static MLKPackage *cl;
static MLKPackage *sys;
static MLKSymbol *IF;
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
static MLKSymbol *_DEFMACRO;


@implementation MLKInterpreter
+(void) initialize
{
  cl = [MLKPackage findPackage:@"COMMON-LISP"];
  sys = [MLKPackage findPackage:@"TOILET-SYSTEM"];

  IF = [cl intern:@"IF"];
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
  _DEFMACRO = [sys intern:@"%DEFMACRO"];
}


+(id) eval:(id)program
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
          return [lexenv valueForSymbol:program];
        }
      else
        {
          //NSLog (@"Processing special variable.");
          return [dynamicContext valueForSymbol:program];
        }
    }
  else if (![program isKindOfClass:[MLKCons class]])
    {
      // Everything that is not a list or a symbol evaluates to itself.
      return program;
    }
  else
    {
      id car = [program car];

      if ([car isKindOfClass:[MLKSymbol class]])
        {
          if (car == APPLY)
            {
              MLKCons *rest = [self eval:[[[program cdr] cdr] car]
                                    inLexicalContext:context
                                    withEnvironment:lexenv];

              return [[[program cdr] car] applyToArray:(rest
                                                        ? (id)[rest array]
                                                        : (id)[NSArray array])];
            }
          else if (car == EVAL)
            {
              return [self eval:[self eval:[program cdr]
                                      inLexicalContext:context
                                      withEnvironment:lexenv]
                           inLexicalContext:[MLKLexicalContext globalContext]
                           withEnvironment:[MLKLexicalEnvironment
                                             globalEnvironment]];
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
              return [[program cdr] car];
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
          else
            {
              if ([context symbolNamesFunction:car])
                {
                  id function = [lexenv functionForSymbol:car];
                  MLKCons *rest = [program cdr];
                  NSMutableArray *args = [NSMutableArray array];
                  
                  while (rest)
                    {
                      id result = [self eval:[rest car]
                                        inLexicalContext:context
                                        withEnvironment:lexenv];
                      [args addObject:(result ? (id)result : (id)[NSNull null])];
                      rest = [rest cdr];
                    }
                  
                  return [function applyToArray:args];
                }
              else if ([context symbolNamesMacro:car])
                {
                  id macrofun = [context macroForSymbol:car];
                  id expansion = [macrofun applyToArray:
                                             [NSArray arrayWithObjects:
                                                        program, context, nil]];
                  return [self eval:expansion
                               inLexicalContext:context
                               withEnvironment:lexenv];
                }
              else
                {
                  [NSException raise:@"MLKNoSuchOperatorException"
                               format:@"%@ does not name a known operator.",
                                      [car descriptionForLisp]];
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
