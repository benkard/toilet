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
#import "util.h"
#import "special-symbols.h"

#import <Foundation/NSString.h>


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


@implementation MLKFunctionCallForm
@end


@implementation MLKCatchForm
@end


@implementation MLKSimpleDefmacroForm
@end


@implementation MLKEvalWhenForm
@end


@implementation MLKForeignLambdaForm
@end


@implementation MLKFunctionForm
@end


@implementation MLKIfForm
@end


@implementation MLKInPackageForm
@end


@implementation MLKSimpleLambdaForm
@end


@implementation MLKSimpleMacroletForm
@end


@implementation MLKSimpleFletForm
@end


@implementation MLKLetForm
@end


@implementation MLKSimpleLoopForm
@end


@implementation MLKMultipleValueCallForm
@end


@implementation MLKProgNForm
@end


@implementation MLKProgVForm
@end


@implementation MLKQuoteForm
@end


@implementation MLKSetQForm
@end


@implementation MLKFSetQForm
@end


@implementation MLKSetForm
@end


@implementation MLKFSetForm
@end


@implementation MLKThrowForm
@end


@implementation MLKUnwindProtectForm
@end
