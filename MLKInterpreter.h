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

#import "MLKCharacterStream.h"
#import "MLKForm.h"
#import "MLKLexicalContext.h"
#import "MLKLexicalEnvironment.h"

#import <Foundation/NSArray.h>
#import <Foundation/NSObject.h>


enum MLKProcessingMode
{
  // Compiler
  compile_time_too_mode,
  not_compile_time_mode,
  expand_mode,

  // Evaluator
  eval_mode
};


@interface MLKInterpreter : NSObject
+(void) initialize;
  
+(NSArray*) eval:(id)program
            inLexicalContext:(MLKLexicalContext *)context
            withEnvironment:(MLKLexicalEnvironment *)lexenv;

+(BOOL) load:(MLKCharacterStream *)stream verbose:(BOOL)verbose print:(BOOL)print;

+(id) compile:(id)object
    inContext:(MLKLexicalContext *)context;
@end


@interface MLKForm (MLKInterpretation)
-(NSArray *) interpret;
-(NSArray *) interpretWithEnvironment:(MLKLexicalEnvironment *)env;
-(NSArray *) reallyInterpretWithEnvironment:(MLKLexicalEnvironment *)env;
@end

@interface MLKBodyForm (MLKInterpretation)
-(NSArray *) interpretBodyWithEnvironment:(MLKLexicalEnvironment *)env;
@end
