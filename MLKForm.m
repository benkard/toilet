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
#import "MLKInterpreter.h"
#import "util.h"
#import "special-symbols.h"

#import <Foundation/NSArray.h>
#import <Foundation/NSSet.h>
#import <Foundation/NSString.h>

#ifdef __OBJC_GC__
#import <Foundation/NSZone.h>
#endif

#include <stdlib.h>

#define MAKE_FORM(OBJECT)            \
  [MLKForm formWithObject:OBJECT     \
                inContext:_context   \
              forCompiler:_compiler]

@implementation MLKForm
+(void) initialize
{
  ensure_symbols ();
}

-(id) initWithObject:(id)object
           inContext:(MLKLexicalContext *)context
         forCompiler:(id)compiler
{
  self = [super init];
  LASSIGN (_form, object);
  LASSIGN (_context, context);
  LASSIGN (_compiler, compiler);

  //NSLog (@"Processing: %@", MLKPrintToString(_form));
  id processed_form = [self complete];
  //NSLog (@"Processing finished: %@", MLKPrintToString(_form));

  return processed_form;
}

-(id) complete
{
  return self;
}

+(Class) dispatchClassForObject:(id)object
{
  if (MLKInstanceP (object) && [object isKindOfClass:[MLKCons class]])
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

-(NSArray *) subforms
{
  return [NSArray array];
}

-(NSSet *) freeVariables
{
  NSMutableSet *freeVariables = [NSMutableSet set];
  NSArray *subforms = [self subforms];
  int i;

  for (i = 0; i < [subforms count]; i++)
    {
      MLKForm *subform = [subforms objectAtIndex:i];
      [freeVariables unionSet:[subform freeVariables]];
    }

  return freeVariables;
}

-(MLKLexicalContext *) context
{
  return _context;
}

-(void) dealloc
{
  LDESTROY (_form);
  LDESTROY (_context);
  LDESTROY (_compiler);
  [super dealloc];
}
@end


@implementation MLKAtomicForm
+(Class) dispatchClassForObject:(id)object
{
  if (MLKInstanceP (object) && [object isKindOfClass:[MLKSymbol class]])
    return [MLKSymbolForm class];
  else
    return [MLKSelfEvaluatingForm class];
}
@end


@implementation MLKSelfEvaluatingForm
+(Class) dispatchClassForObject:(id)object
{
  return self;
}
@end


@implementation MLKSymbolForm
+(Class) dispatchClassForObject:(id)object
{
  return self;
}

-(NSSet *) freeVariables
{
  if ([_context variableIsLexical:_form])
    return [NSSet setWithObject:nullify(_form)];
  else
    return [NSSet set];
}
@end


@implementation MLKCompoundForm
-(id) complete
{
  self = [super complete];
  LASSIGN (_head, [_form car]);
  LASSIGN (_tail, [_form cdr]);
  return self;
}

+(Class) dispatchClassForObject:(id)object
{
  id car = [object car];

  if (car == CATCH) return [MLKCatchForm class];
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
  else if (car == MULTIPLE_VALUE_LIST) return [MLKMultipleValueListForm class];
  else if (car == PROGN) return [MLKProgNForm class];
  else if (car == PROGV) return [MLKProgVForm class];
  else if (car == QUOTE) return [MLKQuoteForm class];
  else if (car == SETQ) return [MLKSetQForm class];
  else if (car == _FSETQ) return [MLKFSetQForm class];
  else if (car == THROW) return [MLKThrowForm class];
  else if (car == UNWIND_PROTECT) return [MLKUnwindProtectForm class];
  else return [MLKSimpleCompoundForm class];
}

-(void) dealloc
{
  LDESTROY (_head);
  LDESTROY (_tail);
  [super dealloc];
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

  if ([self class] != [MLKSimpleCompoundForm class])
    {
      return self;
    }
  else if ([_head isKindOfClass:[MLKCons class]])
    {
      LRELEASE (self);
      return LRETAIN ([MLKForm formWithObject:[MLKCons cons:FUNCALL
                                                       with:object]
                               inContext:context
                               forCompiler:compiler]);
    }
  else if ([context symbolNamesMacro:_head])
    {
      LRELEASE (self);
      return LRETAIN ([MLKMacroCallForm formWithObject:object
                                        inContext:context
                                        forCompiler:compiler]);
    }
  else
    {
      LRELEASE (self);
      return LRETAIN ([MLKFunctionCallForm formWithObject:object
                                           inContext:context
                                           forCompiler:compiler]);
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

  id expansion;

  id <MLKFuncallable> macrofun = [context macroForSymbol:_head];
  //NSLog (@"Expanding: %@", MLKPrintToString (_form));
  NSArray *expansion_values = [macrofun applyToArray:
                                          [NSArray arrayWithObjects:
                                                     _form, context, nil]];
  if ([expansion_values count] > 0)
    {
      expansion = denullify ([expansion_values objectAtIndex:0]);
    }
  else
    {
      expansion = nil;
    }
  //NSLog (@"=> %@", MLKPrintToString (expansion));

  return LRETAIN ([MLKForm formWithObject:expansion
                           inContext:context
                           forCompiler:compiler]);
}
@end


@implementation MLKBodyForm
-(void) splitDeclarationsAndBody:(id)object
{
  LASSIGN (_body, object);
}

-(void) processBody:(id)object inContext:(MLKLexicalContext *)context
{
  id rest;
  NSMutableArray *bodyForms = [NSMutableArray array];

  [self splitDeclarationsAndBody:object];
  rest = _body;
  while (rest)
    {
      [bodyForms addObject:[MLKForm formWithObject:[rest car]
                                         inContext:context
                                       forCompiler:_compiler]];
      rest = [rest cdr];
    }

  LASSIGN (_bodyContext, context);
  LASSIGN (_bodyForms, bodyForms);
}

-(void) processBody:(id)object
{
  [self processBody:object inContext:_context];
}

-(NSArray *) subforms
{
  return _bodyForms;
}

-(NSArray *) bodyForms
{
  return _bodyForms;
}

-(MLKLexicalContext *) bodyContext
{
  return _bodyContext;
}

-(void) dealloc
{
  LDESTROY (_body);
  LDESTROY (_bodyForms);
  LDESTROY (_bodyContext);
  [super dealloc];
}
@end


@implementation MLKDeclaringForm
-(void) splitDeclarationsAndBody:(id)object
{
  id rest;

  LASSIGN (_body, nil);

  MLKSplitDeclarationsDocAndForms(&_declarations, NULL, &_body, object, NO);
  LRETAIN (_declarations);
  LRETAIN (_body);

  _declarationForms = [[NSMutableArray alloc] init];

  for (rest = _declarations; rest; rest = [rest cdr])
    {
      [(NSMutableArray*)_declarationForms addObject:[MLKDeclarationForm
                                                      formWithObject:[rest car]
                                                      inContext:_context
                                                      forCompiler:_compiler]];
    }
}

-(id) declarationsWithForms:(id)object
{
  [self splitDeclarationsAndBody:object];
  return _declarations;
}

-(NSArray *) subforms
{
  return [[super subforms] arrayByAddingObjectsFromArray:_declarationForms];
}

-(void) dealloc
{
  LDESTROY (_declarations);
  LDESTROY (_declarationForms);
  [super dealloc];
}
@end


@implementation MLKDocstringForm
-(void) splitDeclarationsAndBody:(id)object
{
  id rest;

  LASSIGN (_body, nil);

  MLKSplitDeclarationsDocAndForms(&_declarations, &_documentation, &_body, object, YES);
  LRETAIN (_declarations);
  LRETAIN (_body);

  _declarationForms = [[NSMutableArray alloc] init];

  for (rest = _declarations; rest; rest = [rest cdr])
    {
      [(NSMutableArray*)_declarationForms addObject:[MLKDeclarationForm
                                                      formWithObject:[rest car]
                                                      inContext:_context
                                                      forCompiler:_compiler]];
    }
}

-(void) dealloc
{
  LDESTROY (_documentation);
  [super dealloc];
}
@end


@implementation MLKFunctionCallForm
-(id) complete
{
  self = [super complete];

  id rest;
  NSMutableArray *argumentForms = [NSMutableArray array];

  rest = [_form cdr];
  while (rest)
    {
      [argumentForms addObject:[MLKForm formWithObject:[rest car]
                                        inContext:_context
                                        forCompiler:_compiler]];
      rest = [rest cdr];
    }

  LASSIGN (_argumentForms, argumentForms);
  return self;
}

-(NSArray *) subforms
{
  return [[super subforms] arrayByAddingObjectsFromArray:_argumentForms];
}

-(void) dealloc
{
  LDESTROY (_argumentForms);
  [super dealloc];
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

-(NSArray *) subforms
{
  return [[[super subforms] arrayByAddingObject:_tagForm]
          arrayByAddingObjectsFromArray:_bodyForms];
}

-(void) dealloc
{
  LDESTROY (_tagForm);
  [super dealloc];
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

  [self processBody:[_tail cdr]];
  return self;
}
@end


@implementation MLKForeignLambdaForm
-(id) complete
{
  id argtypes;
  int i;

  self = [super complete];
  LASSIGN (_foreignName, [_tail car]);
  LASSIGN (_foreignLibraryDesignator, [[_tail cdr] car]);
  _returnType = MLKForeignTypeWithTypeDesignator ([[[[_tail cdr] cdr] cdr] car]);

  argtypes = [[[_tail cdr] cdr] car];

  _argc = [argtypes length];

#ifdef __OBJC_GC__
  _argumentTypes = NSAllocateCollectable (_argc * sizeof (MLKForeignType), NSScannedOption);
#else
  _argumentTypes = malloc (_argc * sizeof (MLKForeignType));
#endif

  while (argtypes)
    {
      _argumentTypes[i] = MLKForeignTypeWithTypeDesignator ([argtypes car]);
      argtypes = [argtypes cdr];
    }

  return self;
}

-(void) dealloc
{
  LDESTROY (_foreignName);
  LDESTROY (_foreignLibraryDesignator);
  free (_argumentTypes);
  [super dealloc];
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

-(void) dealloc
{
  LDESTROY (_lambdaList);
  [super dealloc];
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

-(NSArray *) subforms
{
  return [[super subforms] arrayByAddingObject:_lambdaForm];
}

-(void) dealloc
{
  LDESTROY (_lambdaForm);
  [super dealloc];
}
@end


@implementation MLKSimpleFunctionForm
-(id) complete
{
  self = [super complete];
  LASSIGN (_functionName, [_tail car]);
  return self;
}

-(void) dealloc
{
  LDESTROY (_functionName);
  [super dealloc];
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

-(NSArray *) subforms
{
  return [[[[super subforms] arrayByAddingObject:_conditionForm]
           arrayByAddingObject:_consequentForm]
          arrayByAddingObject:_alternativeForm];
}

-(void) dealloc
{
  LDESTROY (_conditionForm);
  LDESTROY (_consequentForm);
  LDESTROY (_alternativeForm);
  [super dealloc];
}
@end


@implementation MLKInPackageForm
-(id) complete
{
  self = [super complete];
  LASSIGN (_packageDesignator, [_tail car]);
  return self;
}

-(void) dealloc
{
  LDESTROY (_packageDesignator);
  [super dealloc];
}
@end


@implementation MLKSimpleLambdaForm
-(id) complete
{
  MLKLexicalContext *newContext;
  
  self = [super complete];
  
  LASSIGN (_lambdaListName, [_tail car]);
  newContext = [MLKLexicalContext contextWithParent:_context
                                          variables:[NSSet setWithObject:nullify(_lambdaListName)]
                                          functions:nil
                                             goTags:nil
                                             macros:nil
                                     compilerMacros:nil
                                       symbolMacros:nil
                                       declarations:[self declarationsWithForms:[_tail cdr]]];
  
  [self processBody:[_tail cdr]
          inContext:newContext];
  return self;
}

-(MLKSymbol *) lambdaListName
{
  return _lambdaListName;
}

-(NSSet *) freeVariables
{
  id superfree = [super freeVariables];
  if ([superfree containsObject:_lambdaListName])
    {
      NSMutableSet *freeVariables = [superfree mutableCopy];
      [freeVariables removeObject:_lambdaListName];
      return freeVariables;
    }
  else
    {
      return superfree;
    }
}

-(void) dealloc
{
  LDESTROY (_lambdaListName);
  [super dealloc];
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
                                       declarations:[self declarationsWithForms:[_tail cdr]]];

  newForm = [MLKForm formWithObject:[MLKCons cons:LET
                                             with:[MLKCons cons:nil
                                                           with:[_tail cdr]]]
                          inContext:newContext
                        forCompiler:_compiler];
  LRELEASE (self);  //?FIXME
  return LRETAIN (newForm);
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
                                       declarations:[self declarationsWithForms:[[_tail cdr] cdr]]];

  LASSIGN (_functionBindingForms, bindingForms);
  [self processBody:[_tail cdr]
          inContext:newContext];
  return self;
}

-(NSArray *) subforms
{
  return [[super subforms] arrayByAddingObjectsFromArray:_functionBindingForms];
}

-(void) dealloc
{
  LDESTROY (_functionBindingForms);
  [super dealloc];
}
@end


@implementation MLKLetForm
-(id) complete
{
  NSMutableArray *bindingForms;
  MLKCons *bindings;
  NSMutableSet *variables;
  MLKLexicalContext *newContext;
  
  self = [super complete];
  
  bindingForms = [NSMutableArray array];
  variables = [NSMutableSet set];
  bindings = [_tail car];
  
  while (bindings)
    {
      id varname;

      if ([[bindings car] isKindOfClass:[MLKCons class]])
        {
          varname = [[bindings car] car];
        }
      else
        {
          varname = [bindings car];
        }

      [bindingForms addObject:[MLKVariableBindingForm formWithObject:[bindings car]
                                                           inContext:_context
                                                         forCompiler:_compiler]];
      [variables addObject:varname];
      bindings = [bindings cdr];
    }
  
  newContext = [MLKLexicalContext contextWithParent:_context
                                          variables:variables
                                          functions:nil
                                             goTags:nil
                                             macros:nil
                                     compilerMacros:nil
                                       symbolMacros:nil
                                       declarations:[self declarationsWithForms:[[_tail cdr] cdr]]];
  
  LASSIGN (_variableBindingForms, bindingForms);
  [self processBody:[_tail cdr]
          inContext:newContext];
  return self;
}

-(NSArray *) subforms
{
  return [[super subforms] arrayByAddingObjectsFromArray:_variableBindingForms];
}

-(NSSet *) freeVariables
{
  NSMutableSet *freeVariables = [[super freeVariables] mutableCopy];
  int i;

  for (i = 0; i < [_variableBindingForms count]; i++)
    {
      id symbol = [[_variableBindingForms objectAtIndex:i] name];
      if ([freeVariables containsObject:symbol]
          && [_bodyContext variableIsLexical:symbol])
        {
          [freeVariables removeObject:symbol];
        }
    }

  return freeVariables;
}

-(void) dealloc
{
  LDESTROY (_variableBindingForms);
  [super dealloc];
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
                                       declarations:[self declarationsWithForms:[[_tail cdr] cdr]]];
  
  [self processBody:[_tail cdr]];
  return self;
}
@end


@implementation MLKSimpleLoopForm
-(id) complete
{
  self = [super complete];
  [self processBody:_tail];
  return self;
}
@end


@implementation MLKMultipleValueListForm
-(id) complete
{
  self = [super complete];
  LASSIGN (_listForm, [MLKForm formWithObject:[_tail car]
                                    inContext:_context
                                  forCompiler:_compiler]);
  return self;
}

-(NSArray *) subforms
{
  return [[super subforms] arrayByAddingObject:_listForm];
}

-(void) dealloc
{
  LDESTROY (_listForm);
  [super dealloc];
}
@end


@implementation MLKProgNForm
-(id) complete
{
  self = [super complete];
  [self processBody:_tail];
  return self;
}
@end


@implementation MLKProgVForm
-(id) complete
{
  self = [super complete];
  LASSIGN (_variableListForm, MAKE_FORM ([_tail car]));
  LASSIGN (_valueListForm, MAKE_FORM ([[_tail cdr] car]));
  [self processBody:[[_tail cdr] cdr]];
  return self;
}

-(NSArray *) subforms
{
  return [[[super subforms] arrayByAddingObject:_variableListForm]
          arrayByAddingObject:_valueListForm];
}

-(void) dealloc
{
  LDESTROY (_variableListForm);
  LDESTROY (_valueListForm);
  [super dealloc];
}
@end


@implementation MLKQuoteForm
-(id) complete
{
  self = [super complete];
  LASSIGN (_quotedData, [_tail car]);
  return self;
}

-(void) dealloc
{
  LDESTROY (_quotedData);
  [super dealloc];
}
@end


@implementation MLKSetQForm
-(id) complete
{
  id rest;
  NSMutableArray *variables, *valueForms;

  self = [super complete];

  rest = _tail;
  variables = [NSMutableArray array];
  valueForms = [NSMutableArray array];
  while (rest)
    {
      [variables addObject:nullify([rest car])];
      [valueForms addObject:MAKE_FORM([[rest cdr] car])];
      rest = [[rest cdr] cdr];
    }

  LASSIGN (_variables, variables);
  LASSIGN (_valueForms, valueForms);
  return self;
}

-(NSArray *) subforms
{
  return [[super subforms] arrayByAddingObjectsFromArray:_valueForms];
}

-(void) dealloc
{
  LDESTROY (_variables);
  LDESTROY (_valueForms);
  [super dealloc];
}
@end


@implementation MLKFSetQForm
-(id) complete
{
  id rest;
  NSMutableArray *functionNames, *valueForms;
  
  self = [super complete];
  
  rest = _tail;
  functionNames = [NSMutableArray array];
  valueForms = [NSMutableArray array];
  while (rest)
    {
      [functionNames addObject:nullify([rest car])];
      [valueForms addObject:MAKE_FORM([[rest cdr] car])];
      rest = [[rest cdr] cdr];
    }
  
  LASSIGN (_functionNames, functionNames);
  LASSIGN (_valueForms, valueForms);
  return self;
}

-(NSArray *) subforms
{
  return [[super subforms] arrayByAddingObjectsFromArray:_valueForms];
}

-(void) dealloc
{
  LDESTROY (_functionNames);
  LDESTROY (_valueForms);
  [super dealloc];
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

-(NSArray *) subforms
{
  return [[[super subforms] arrayByAddingObject:_tagForm]
          arrayByAddingObject:_valueForm];
}

-(void) dealloc
{
  LDESTROY (_tagForm);
  LDESTROY (_valueForm);
  [super dealloc];
}
@end


@implementation MLKUnwindProtectForm
-(id) complete
{
  self = [super complete];
  LASSIGN (_protectedForm, MAKE_FORM ([_tail car]));
  [self processBody:[_tail cdr]];
  return self;
}

-(NSArray *) subforms
{
  return [[super subforms] arrayByAddingObject:_protectedForm];
}

-(void) dealloc
{
  LDESTROY (_protectedForm);
  [super dealloc];
}
@end


@implementation MLKSimpleFunctionBindingForm
+(Class) dispatchClassForObject:(id)object
{
  return self;
}

-(id) complete
{
  MLKLexicalContext *newContext;

  self = [super complete];

  LASSIGN (_name, _head);
  LASSIGN (_lambdaListName, [_tail car]);

  newContext = [MLKLexicalContext contextWithParent:_context
                                          variables:[NSSet setWithObject:nullify(_lambdaListName)]
                                          functions:nil
                                             goTags:nil
                                             macros:nil
                                     compilerMacros:nil
                                       symbolMacros:nil
                                       declarations:[self declarationsWithForms:[_tail cdr]]];

  [self processBody:[_tail cdr]
          inContext:newContext];
  return self;
}

-(id) name
{
  return _name;
}

-(id) lambdaListName
{
  return _lambdaListName;
}

-(id) bodyForms
{
  return _bodyForms;
}

-(void) dealloc
{
  LDESTROY (_name);
  LDESTROY (_lambdaListName);
  [super dealloc];
}
@end


@implementation MLKVariableBindingForm
+(Class) dispatchClassForObject:(id)object
{
  return self;
}

-(id) complete
{
  self = [super complete];

  if ([_form isKindOfClass:[MLKCons class]])
    {
      LASSIGN (_name, [_form car]);
      LASSIGN (_valueForm, MAKE_FORM ([[_form cdr] car]));
    }
  else
    {
      LASSIGN (_name, _form);
      LASSIGN (_valueForm, MAKE_FORM (nil));
    }

  return self;
}

-(NSArray *) subforms
{
  return [[super subforms] arrayByAddingObject:_valueForm];
}

-(id) name
{
  return _name;
}

-(id) valueForm
{
  return _valueForm;
}

-(void) dealloc
{
  LDESTROY (_name);
  LDESTROY (_valueForm);
  [super dealloc];
}
@end


@implementation MLKDeclarationForm : MLKCompoundForm
+(Class) dispatchClassForObject:(id)object
{
  return self;
}

-(id) complete
{
  self = [super complete];

  LASSIGN (_type, [_form car]);
  LASSIGN (_arguments, [_form cdr] ? (id)[[_form cdr] array] : (id)[NSArray array]);

  return self;
}

-(void) dealloc
{
  LDESTROY (_type);
  LDESTROY (_arguments);
  [super dealloc];
}
@end
