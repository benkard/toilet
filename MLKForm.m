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

#import "MLKCons.h"
#import "MLKForm.h"
#import "MLKLLVMCompiler.h"
#import "util.h"
#import "special-symbols.h"

#import <Foundation/NSArray.h>
#import <Foundation/NSSet.h>
#import <Foundation/NSString.h>

#include <stdlib.h>

#define MAKE_FORM(OBJECT)            \
  [MLKForm formWithObject:OBJECT     \
                inContext:_context   \
              forCompiler:_compiler]

@implementation MLKForm
-(void) initialize
{
  ensure_symbols ();
}

-(id) initWithObject:(id)object
           inContext:(MLKLexicalContext *)context
         forCompiler:(id)compiler
{
  _form = object;
  _context = context;
  _compiler = compiler;
  return [self complete];
}

-(id) complete
{
  return self;
}

+(Class) dispatchClassForObject:(id)object
{
  if ([object isKindOfClass:[MLKCons class]])
    return [MLKCompoundForm class];
  else
    return [MLKAtomicForm class];
}

+(id) formWithObject:(id)object
           inContext:(MLKLexicalContext *)context
         forCompiler:(id)compiler
{
  Class cls = [self dispatchClassForObject:object];
                
  if (cls != self)
    return [cls formWithObject:object
                inContext:context
                forCompiler:compiler];
  else
    return LAUTORELEASE ([[self alloc]
                           initWithObject:object
                           inContext:context
                           forCompiler:compiler]);
}
@end


@implementation MLKAtomicForm
+(Class) dispatchClassForObject:(id)object
{
  if ([object isKindOfClass:[MLKSymbol class]])
    return [MLKSymbolForm class];
  else
    return [MLKSelfEvaluatingForm class];
}
@end


@implementation MLKSelfEvaluatingForm
// FIXME

+(Class) dispatchClassForObject:(id)object
{
  return self;
}
@end


@implementation MLKSymbolForm
// FIXME

+(Class) dispatchClassForObject:(id)object
{
  return self;
}
@end


@implementation MLKCompoundForm
-(id) complete
{
  self = [super complete];
  _head = [_form car];
  _tail = [_form cdr];
  return self;
}

+(Class) dispatchClassForObject:(id)object
{
  id car = [object car];

  if (car == APPLY) return [MLKFunctionCallForm class];
  else if (car == CATCH) return [MLKCatchForm class];
  else if (car == _DEFMACRO) return [MLKSimpleDefmacroForm class];
  else if (car == EVAL) return [MLKFunctionCallForm class];
  else if (car == EVAL_WHEN) return [MLKEvalWhenForm class];
  else if (car == _FOREIGN_LAMBDA) return [MLKForeignLambdaForm class];
  else if (car == FUNCTION) return [MLKFunctionForm class];
  else if (car == IF) return [MLKIfForm class];
  else if (car == IN_PACKAGE) return [MLKInPackageForm class];
  else if (car == _LAMBDA) return [MLKSimpleLambdaForm class];
  else if (car == _MACROLET) return [MLKSimpleMacroletForm class];
  else if (car == _FLET) return [MLKSimpleFletForm class];
  else if (car == LET) return [MLKLetForm class];
  else if (car == _LOOP) return [MLKSimpleLoopForm class];
  else if (car == MULTIPLE_VALUE_CALL) return [MLKMultipleValueCallForm class];
  else if (car == PROGN) return [MLKProgNForm class];
  else if (car == PROGV) return [MLKProgVForm class];
  else if (car == QUOTE) return [MLKQuoteForm class];
  else if (car == SETQ) return [MLKSetQForm class];
  else if (car == _FSETQ) return [MLKFSetQForm class];
  else if (car == SET) return [MLKSetForm class];
  else if (car == _FSET) return [MLKFSetForm class];
  else if (car == THROW) return [MLKThrowForm class];
  else if (car == UNWIND_PROTECT) return [MLKUnwindProtectForm class];
  else return [MLKSimpleCompoundForm class];
}
@end


@implementation MLKSimpleCompoundForm
-(id) initWithObject:(id)object
           inContext:(MLKLexicalContext *)context
         forCompiler:(id)compiler
{
  self = [super initWithObject:object
                inContext:context
                forCompiler:compiler];

  if ([_head isKindOfClass:[MLKCons class]])
    {
      LRELEASE (self);
      return [MLKForm formWithObject:[MLKCons cons:FUNCALL
                                              with:object]
                      inContext:context
                      forCompiler:compiler];
    }
  else if ([context symbolNamesMacro:_head])
    {
      LRELEASE (self);
      return [MLKMacroCallForm formWithObject:object
                               inContext:context
                               forCompiler:compiler];
    }
  else
    {
      LRELEASE (self);
      return [MLKFunctionCallForm formWithObject:object
                                  inContext:context
                                  forCompiler:compiler];
    }
}


+(Class) dispatchClassForObject:(id)object
{
  return self;
}
@end


@implementation MLKMacroCallForm
-(id) initWithObject:(id)object
           inContext:(MLKLexicalContext *)context
         forCompiler:(id)compiler
{
  self = [super initWithObject:object
                inContext:context
                forCompiler:compiler];

  id <MLKFuncallable> macrofun = [context macroForSymbol:_head];
  id expansion = denullify ([[macrofun
                               applyToArray:
                                 [NSArray arrayWithObjects:
                                            _form, context, nil]]
                              objectAtIndex:0]);

  return [MLKForm formWithObject:expansion
                  inContext:context
                  forCompiler:compiler];
}
@end


@implementation MLKBodyForm
@end


@implementation MLKDeclaringForm
@end


@implementation MLKDocstringForm
@end


@implementation MLKFunctionCallForm
// -(id <MLKFuncallable>) functionInfo
// {
//   return [_context functionInfoForSymbol:_head];
// }

-(id) complete
{
  self = [super complete];
  LASSIGN (_argumentForms, [_tail array]);
  return self;
}
@end


@implementation MLKCatchForm
-(id) complete
{
  self = [super complete];
  LASSIGN (_tagForm, [_tail car]);
  LASSIGN (_bodyForms, [[_tail cdr] array]);
  return self;
}
@end


@implementation MLKSimpleDefmacroForm
-(id) complete
{
  MLKLexicalContext *newContext;

  self = [super complete];

  LASSIGN (_name, [_tail car]);
  LASSIGN (_lambdaListName, [[_tail cdr] car]);
  newContext = [MLKLexicalContext contextWithParent:_context
                                          variables:[NSSet setWithObject:_lambdaListName]
                                          functions:nil
                                             goTags:nil
                                             macros:nil
                                     compilerMacros:nil
                                       symbolMacros:nil
                                       declarations:[self processDeclarations:[[_tail cdr] cdr]]];

  [self processBody:[[_tail cdr] cdr]
          inContext:newContext];
  return self;
}
@end


@implementation MLKEvalWhenForm
-(id) complete
{
  id rest;

  self = [super complete];

  rest = [_tail car];
  while (rest)
    {
      _compileToplevel |= ([rest car] == COMPILE_TOPLEVEL);
      _loadToplevel |= ([rest car] == LOAD_TOPLEVEL);
      _execute |= ([rest car] == EXECUTE);
      rest = [rest cdr];
    }

  [self procesRawBody:[_tail cdr]];
  return self;
}
@end


@implementation MLKForeignLambdaForm
-(id) complete
{
  id argtypes;
  int i;

  self = [super complete];
  LASSIGN (_foreignName, [[_tail cdr] car]);
  LASSIGN (_name, [_tail car]);
  _returnType = MLKForeignTypeWithTypeDesignator ([[[_tail cdr] cdr] car]);

  argtypes = [[[_tail cdr] cdr] cdr];

  _argc = [argtypes length];
  _argumentTypes = malloc (_argc * sizeof (MLKForeignType));
  while (argtypes)
    {
      _argumentTypes[i] = MLKForeignTypeWithTypeDesignator ([argtypes car]);
      argtypes = [argtypes cdr];
    }

  return self;
}
@end


@implementation MLKLambdaForm
-(id) complete
{
  self = [super complete];
  LASSIGN (_lambdaList, [_tail car]);

  // FIXME
  [NSException raise:@"MLKNotImplementedError"
              format:@"LAMBDA not yet implemented in the compiler"];
  
  return self;
}
@end


@implementation MLKFunctionForm
+(Class) dispatchClassForObject:(id)object
{
  id funname = [[object cdr] car];
  if ([funname isKindOfClass:[MLKCons class]]
      && [funname car] == LAMBDA)
    return [MLKLambdaFunctionForm class];
  else
    return [MLKSimpleFunctionForm class];
}
@end


@implementation MLKLambdaFunctionForm
-(id) complete
{
  self = [super complete];
  LASSIGN (_lambdaForm, MAKE_FORM ([_tail car]));
  return self;
}
@end


@implementation MLKSimpleFunctionForm
-(id) complete
{
  self = [super complete];
  LASSIGN (_functionName, [_tail car]);
  return self;
}
@end


@implementation MLKIfForm
-(id) complete
{
  self = [super complete];
  LASSIGN (_conditionForm, MAKE_FORM ([_tail car]));
  LASSIGN (_consequentForm, MAKE_FORM ([[_tail cdr] car]));
  LASSIGN (_alternativeForm, MAKE_FORM ([[[_tail cdr] cdr] car]));
  return self;
}
@end


@implementation MLKInPackageForm
-(id) complete
{
  self = [super complete];
  LASSIGN (_packageDesignator, [_tail car]);
  return self;
}
@end


@implementation MLKSimpleLambdaForm
-(id) complete
{
  MLKLexicalContext *newContext;
  
  self = [super complete];
  
  LASSIGN (_lambdaListName, [_tail car]);
  newContext = [MLKLexicalContext contextWithParent:_context
                                          variables:[NSSet setWithObject:_lambdaListName]
                                          functions:nil
                                             goTags:nil
                                             macros:nil
                                     compilerMacros:nil
                                       symbolMacros:nil
                                       declarations:[self processDeclarations:[_tail cdr]]];
  
  [self processBody:[_tail cdr]
          inContext:newContext];
  return self;
}
@end


@implementation MLKSimpleMacroletForm
-(id) initWithObject:(id)object
           inContext:(MLKLexicalContext *)context
         forCompiler:(id)compiler
{
  MLKLexicalContext *newContext;
  MLKForm *newForm;
  NSMutableDictionary *macros;
  id bindings;

  macros = [NSMutableDictionary dictionary];
  bindings = [_tail car];
  while (bindings)
    {
      id macro;
      macro = [_compiler compile:[MLKCons cons:_LAMBDA
                                          with:[[bindings car] cdr]]
                         inContext:_context];
      [macros setObject:macro
                 forKey:nullify ([[bindings car] car])];
      bindings = [bindings cdr];
    }

  newContext = [MLKLexicalContext contextWithParent:_context
                                          variables:nil
                                          functions:nil
                                             goTags:nil
                                             macros:macros
                                     compilerMacros:nil
                                       symbolMacros:nil
                                       declarations:[self processDeclarations:[_tail cdr]]];
  [self processDeclarationsAndBody:[_tail cdr]];
  newForm = [MLKForm formWithObject:[MLKCons cons:LET
                                             with:[MLKCons cons:nil
                                                           with:[_tail cdr]]]
                          inContext:newContext
                        forCompiler:_compiler];
  LDEALLOC (self);
  return newForm;
}
@end


@implementation MLKSimpleFletForm
-(id) complete
{
  NSMutableArray *bindingForms;
  MLKCons *bindings;
  NSMutableSet *functions;
  MLKLexicalContext *newContext;

  self = [super complete];

  bindingForms = [NSMutableArray array];
  functions = [NSMutableSet set];
  bindings = [_tail car];

  while (bindings)
    {
      [bindingForms addObject:[MLKSimpleFunctionBindingForm formWithObject:[bindings car]
                                                                 inContext:_context
                                                               forCompiler:_compiler]];
      [functions addObject:[[bindings car] car]];
      bindings = [bindings cdr];
    }

  newContext = [MLKLexicalContext contextWithParent:_context
                                          variables:nil
                                          functions:functions
                                             goTags:nil
                                             macros:nil
                                     compilerMacros:nil
                                       symbolMacros:nil
                                       declarations:[self processDeclarations:[[_tail cdr] cdr]]];

  LASSIGN (_functionBindingForms, bindingForms);
  [self processBody:[_tail cdr]
          inContext:newContext];
  return self;
}
@end


@implementation MLKLetForm
-(id) complete
{
  MLKLexicalContext *newContext;

  self = [super complete];
  // FIXME
  [self processBody:[_tail cdr]
          inContext:newContext];
  return self;
}
@end


@implementation MLKLocallyForm
-(id) complete
{
  MLKLexicalContext *newContext;

  self = [super complete];
  newContext = [MLKLexicalContext contextWithParent:_context
                                          variables:nil
                                          functions:nil
                                             goTags:nil
                                             macros:nil
                                     compilerMacros:nil
                                       symbolMacros:nil
                                       declarations:[self processDeclarations:[[_tail cdr] cdr]]];
  
  [self processRawBody:[_tail cdr]];
  return self;
}
@end


@implementation MLKSimpleLoopForm
-(id) complete
{
  self = [super complete];
  [self processRawBody:_tail];
  return self;
}
@end


@implementation MLKMultipleValueCallForm
-(id) complete
{
  self = [super complete];
  LASSIGN (_functionForm, [_tail car]);
  [self processRawBody:[_tail cdr]];
  return self;
}
@end


@implementation MLKProgNForm
-(id) complete
{
  self = [super complete];
  [self processRawBody:_tail];
  return self;
}
@end


@implementation MLKProgVForm
-(id) complete
{
  self = [super complete];
  LASSIGN (_variableListForm, MAKE_FORM ([_tail car]));
  LASSIGN (_valueListForm, MAKE_FORM ([[_tail cdr] car]));
  [self processRawBody:[[_tail cdr] cdr]];
  return self;
}
@end


@implementation MLKQuoteForm
-(id) complete
{
  self = [super complete];
  LASSIGN (_quotedData, [_tail car]);
  return self;
}
@end


@implementation MLKSetQForm
-(id) complete
{
  id rest = _tail;
  NSMutableArray *variables, *valueForms;

  self = [super complete];

  variables = [NSMutableArray array];
  valueForms = [NSMutableArray array];
  while (rest)
    {
      [variables addObject:[rest car]];
      [valueForms addObject:MAKE_FORM([[rest cdr] car])];
      rest = [[rest cdr] cdr];
    }

  LASSIGN (_variables, variables);
  LASSIGN (_valueForms, valueForms);
  return self;
}
@end


@implementation MLKFSetQForm
-(id) complete
{
  id rest = _tail;
  NSMutableArray *functionNames, *valueForms;
  
  self = [super complete];
  
  functionNames = [NSMutableArray array];
  valueForms = [NSMutableArray array];
  while (rest)
    {
      [functionNames addObject:[rest car]];
      [valueForms addObject:MAKE_FORM([[rest cdr] car])];
      rest = [[rest cdr] cdr];
    }
  
  LASSIGN (_functionNames, functionNames);
  LASSIGN (_valueForms, valueForms);
  return self;
}
@end


@implementation MLKSetForm
-(id) complete
{
  self = [super complete];
  LASSIGN (_variableForm, MAKE_FORM ([_tail car]));
  LASSIGN (_valueForm, MAKE_FORM ([[_tail cdr] car]));
  return self;
}
@end


@implementation MLKFSetForm
-(id) complete
{
  self = [super complete];
  LASSIGN (_functionNameForm, MAKE_FORM ([_tail car]));
  LASSIGN (_valueForm, MAKE_FORM ([[_tail cdr] car]));
  return self;
}
@end


@implementation MLKThrowForm
-(id) complete
{
  self = [super complete];
  LASSIGN (_tagForm, MAKE_FORM ([_tail car]));
  LASSIGN (_valueForm, MAKE_FORM ([[_tail cdr] car]));
  return self;
}
@end


@implementation MLKUnwindProtectForm
-(id) complete
{
  self = [super complete];
  LASSIGN (_protectedForm, MAKE_FORM ([_tail car]));
  [self processRawBody:[_tail cdr]];
  return self;
}
@end


@implementation MLKSimpleFunctionBindingForm
+(id) formWithObject:(id)object
           inContext:(MLKLexicalContext *)context
         forCompiler:(id)compiler
{
  return [[self alloc] initWithObject:(id)object
                            inContext:(MLKLexicalContext *)context
                          forCompiler:(id)compiler];
}

-(id) complete
{
  MLKLexicalContext *newContext;

  self = [super complete];

  LASSIGN (_name, _head);
  LASSIGN (_lambdaListName, [_tail car]);

  newContext = [MLKLexicalContext contextWithParent:_context
                                          variables:[NSSet setWithObject:_lambdaListName]
                                          functions:nil
                                             goTags:nil
                                             macros:nil
                                     compilerMacros:nil
                                       symbolMacros:nil
                                       declarations:[self processDeclarations:[_tail cdr]]];

  [self processBody:[_tail cdr]
          inContext:newContext];
  return self;
}
@end
