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

#import "MLKFuncallable.h"
#import "MLKLispValue.h"
#import "MLKLexicalContext.h"
#import "MLKLexicalEnvironment.h"

#import <Foundation/NSArray.h>
#import <Foundation/NSString.h>


@interface MLKInterpretedClosure : MLKLispValue <MLKFuncallable>
{
  id bodyForm;
  MLKSymbol *lambdaListName;
  MLKLexicalContext *context;
  MLKLexicalEnvironment *environment;
}

-(id) initWithBodyForms:(id)forms
         lambdaListName:(MLKSymbol *)symbol
                context:(MLKLexicalContext *)lexctx
            environment:(MLKLexicalEnvironment *)lexenv;

-(NSArray *) applyToArray:(NSArray *)arguments;

-(NSString *) description;
-(NSString *) descriptionForLisp;
@end
