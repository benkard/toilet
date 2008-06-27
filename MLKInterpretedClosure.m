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
#import "MLKInterpretedClosure.h"
#import "MLKInterpreter.h"
#import "runtime-compatibility.h"

#import <Foundation/NSDictionary.h>
#import <Foundation/NSSet.h>


@implementation MLKInterpretedClosure
-(id) initWithBodyForm:(id)form
        lambdaListName:(MLKSymbol *)symbol
               context:(MLKLexicalContext *)lexctx
           environment:(MLKLexicalEnvironment *)lexenv
{
  self = [super init];
  ASSIGN (bodyForm, form);
  ASSIGN (context, lexctx);
  ASSIGN (environment, lexenv);
  ASSIGN (lambdaListName, symbol);
  return self;
}

-(NSArray *) applyToArray:(NSArray *)arguments
{
  id arglist = [MLKCons listWithArray:arguments];

  MLKLexicalEnvironment *new_environment =
    [MLKLexicalEnvironment environmentWithParent:environment
                           variables:[NSDictionary dictionaryWithObject:arglist
                                                   forKey:lambdaListName]
                           functions:nil];

  MLKLexicalContext *new_context =
    [MLKLexicalContext contextWithParent:context
                       variables:[NSSet setWithObject:lambdaListName]
                       functions:nil
                       goTags:nil
                       macros:nil
                       symbolMacros:nil
                       declarations:nil];

  return [MLKInterpreter eval:bodyForm
                         inLexicalContext:new_context
                         withEnvironment:new_environment];
}

-(NSString *) description
{
  return [self descriptionForLisp];
}

-(NSString *) descriptionForLisp
{
  return [NSString stringWithFormat:@"<Interpreted closure @%p>", self];
}
@end
