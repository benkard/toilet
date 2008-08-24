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

#import "MLKForm.h"
#import "MLKLexicalContext.h"
#import "functions.h"

#import <Foundation/NSObject.h>
#import <Foundation/NSString.h>


@interface MLKForm : NSObject
{
  id _form;
  MLKLexicalContext *_context;
  id _compiler;
}

-(id) initWithObject:(id)object
           inContext:(MLKLexicalContext *)context
         forCompiler:(id)compiler;

-(id) complete;
+(Class) dispatchClassForObject:(id)object;

+(id) formWithObject:(id)object
           inContext:(MLKLexicalContext *)context
         forCompiler:(id)compiler;

-(NSArray *) subforms;
@end


@interface MLKAtomicForm : MLKForm
+(Class) dispatchClassForObject:(id)object;
@end


@interface MLKSelfEvaluatingForm : MLKAtomicForm
+(Class) dispatchClassForObject:(id)object;
@end


@interface MLKSymbolForm : MLKAtomicForm
+(Class) dispatchClassForObject:(id)object;
@end


@interface MLKCompoundForm : MLKForm
{
  id _head;
  id _tail;
}

-(id) complete;
+(Class) dispatchClassForObject:(id)object;
@end

@interface MLKSimpleCompoundForm : MLKCompoundForm
-(id) initWithObject:(id)object
           inContext:(MLKLexicalContext *)context
         forCompiler:(id)compiler;

+(Class) dispatchClassForObject:(id)object;
@end


@interface MLKMacroCallForm : MLKSimpleCompoundForm
-(id) initWithObject:(id)object
           inContext:(MLKLexicalContext *)context
         forCompiler:(id)compiler;
@end


@interface MLKBodyForm : MLKCompoundForm
{
  id _body;
  NSArray *_bodyForms;
  id _bodyContext;
}

-(void) splitDeclarationsAndBody:(id)object;
-(void) processBody:(id)object inContext:(MLKLexicalContext *)context;
-(void) processBody:(id)object;
@end


@interface MLKDeclaringForm : MLKBodyForm
{
  id _declarations;
  NSArray *_declarationForms;
}

-(void) splitDeclarationsAndBody:(id)object;
-(id) declarationsWithForms:(id)object;
@end


@interface MLKDocstringForm : MLKDeclaringForm
{
  NSString *_documentation;
}

-(void) splitDeclarationsAndBody:(id)object;
@end


@interface MLKFunctionCallForm : MLKSimpleCompoundForm
{
  NSArray *_argumentForms;
}
@end


@interface MLKCatchForm : MLKBodyForm
{
  MLKForm *_tagForm;
}
@end


@interface MLKEvalWhenForm : MLKBodyForm
{
  BOOL _compileToplevel;
  BOOL _loadToplevel;
  BOOL _execute;
}
@end


@interface MLKForeignLambdaForm : MLKCompoundForm
{
  NSString *_foreignName;
  MLKSymbol *_foreignLibraryDesignator;
  MLKForeignType _returnType;
  MLKForeignType *_argumentTypes;
  int _argc;
}
@end


@interface MLKLambdaForm : MLKDocstringForm
{
  id _lambdaList;
}
@end


@interface MLKFunctionForm : MLKCompoundForm
+(Class) dispatchClassForObject:(id)object;
@end


@interface MLKLambdaFunctionForm : MLKFunctionForm
{
  MLKLambdaForm *_lambdaForm;
}
@end


@interface MLKSimpleFunctionForm : MLKFunctionForm
{
  id _functionName;
}
@end


@interface MLKIfForm : MLKCompoundForm
{
  MLKForm *_conditionForm;
  MLKForm *_consequentForm;
  MLKForm *_alternativeForm;
}
@end


@interface MLKInPackageForm : MLKCompoundForm
{
  id _packageDesignator;
}
@end


@interface MLKSimpleLambdaForm : MLKDocstringForm
{
  MLKSymbol *_lambdaListName;
}
@end


@interface MLKSimpleMacroletForm : MLKDeclaringForm
-(id) initWithObject:(id)object
           inContext:(MLKLexicalContext *)context
         forCompiler:(id)compiler;
@end


@interface MLKSimpleFletForm : MLKDeclaringForm
{
  NSArray *_functionBindingForms;
}
@end


@interface MLKLetForm : MLKDeclaringForm
{
  NSArray *_variableBindingForms;
}
@end


@interface MLKLocallyForm : MLKDeclaringForm
@end


@interface MLKSimpleLoopForm : MLKBodyForm
@end


@interface MLKMultipleValueCallForm : MLKBodyForm
{
  id _functionForm;
}
@end


@interface MLKProgNForm : MLKBodyForm
@end


@interface MLKProgVForm : MLKBodyForm
{
  MLKForm *_variableListForm;
  MLKForm *_valueListForm;
}
@end


@interface MLKQuoteForm : MLKCompoundForm
{
  id _quotedData;
}
@end


@interface MLKSetQForm : MLKCompoundForm
{
  NSArray *_variables;
  NSArray *_valueForms;
}
@end


@interface MLKFSetQForm : MLKCompoundForm
{
  NSArray *_functionNames;
  NSArray *_valueForms;
}
@end


@interface MLKThrowForm : MLKCompoundForm
{
  MLKForm *_tagForm;
  MLKForm *_valueForm;
}
@end


@interface MLKUnwindProtectForm : MLKBodyForm
{
  MLKForm *_protectedForm;
}
@end


@interface MLKSimpleFunctionBindingForm : MLKDocstringForm
{
  id _name;
  MLKSymbol *_lambdaListName;
}

+(Class) dispatchClassForObject:(id)object;

-(id) name;
-(id) lambdaListName;
-(id) bodyForms;
@end


@interface MLKVariableBindingForm : MLKForm
{
  id _name;
  MLKForm *_valueForm;
}

+(Class) dispatchClassForObject:(id)object;

-(id) name;
-(id) valueForm;
@end


@interface MLKDeclarationForm : MLKCompoundForm
{
  id _type;
  NSArray *_arguments;
}

+(Class) dispatchClassForObject:(id)object;
@end
