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


@interface MLKFunctionCallForm : MLKSimpleCompoundForm
@end


@interface MLKCatchForm : MLKCompoundForm
@end


@interface MLKSimpleDefmacroForm : MLKCompoundForm
@end


@interface MLKEvalWhenForm : MLKCompoundForm
@end


@interface MLKForeignLambdaForm : MLKCompoundForm
@end


@interface MLKFunctionForm : MLKCompoundForm
@end


@interface MLKIfForm : MLKCompoundForm
@end


@interface MLKInPackageForm : MLKCompoundForm
@end


@interface MLKSimpleLambdaForm : MLKCompoundForm
@end


@interface MLKSimpleMacroletForm : MLKCompoundForm
@end


@interface MLKSimpleFletForm : MLKCompoundForm
@end


@interface MLKLetForm : MLKCompoundForm
@end


@interface MLKSimpleLoopForm : MLKCompoundForm
@end


@interface MLKMultipleValueCallForm : MLKCompoundForm
@end


@interface MLKProgNForm : MLKCompoundForm
@end


@interface MLKProgVForm : MLKCompoundForm
@end


@interface MLKQuoteForm : MLKCompoundForm
@end


@interface MLKSetQForm : MLKCompoundForm
@end


@interface MLKFSetQForm : MLKCompoundForm
@end


@interface MLKSetForm : MLKCompoundForm
@end


@interface MLKFSetForm : MLKCompoundForm
@end


@interface MLKThrowForm : MLKCompoundForm
@end


@interface MLKUnwindProtectForm : MLKCompoundForm
@end
