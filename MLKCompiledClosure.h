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

#import "MLKFuncallable.h"
#import "MLKLexicalContext.h"
#import "MLKLexicalEnvironment.h"

#import <Foundation/NSArray.h>
#import <Foundation/NSString.h>


@interface MLKCompiledClosure : NSObject <MLKFuncallable>
{
@public
  int m_dataLength;
  id (*m_code)();
  id *m_data;
}

// Why intptr_t?  Because it makes it easier to call this method from
// LLVM-generated code without proper type handling.
-(id) initWithCode:(void *)code
              data:(id *)data
            length:(intptr_t)dataLength;

+(id) closureWithCode:(void *)code
                 data:(id *)data
               length:(intptr_t)dataLength;;

-(NSArray *) applyToArray:(NSArray *)arguments;

-(NSString *) description;
-(NSString *) descriptionForLisp;

-(id (*)()) code;
-(void *) closureData;

-(void) dealloc;
@end
