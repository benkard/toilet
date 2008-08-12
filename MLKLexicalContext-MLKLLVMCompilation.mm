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

#import "MLKLexicalContext-MLKLLVMCompilation.h"

#import <Foundation/NSString.h>
#import <Foundation/NSValue.h>

#include <vector>
#include <llvm/Value.h>
#include <llvm/BasicBlock.h>
using namespace llvm;
using namespace std;


@implementation MLKLexicalContext (MLKLLVMCompilation)
-(void) setVariableHeapAllocation:(BOOL)heapp
                        forSymbol:(id)name
{
  [self setDeepProperty:[NSNumber numberWithBool:heapp]
        forVariable:name
        key:@"LLVM.heap-flag"];
}

-(BOOL) variableHeapAllocationForSymbol:(id)name;
{
  id flag = [self deepPropertyForVariable:name
                  key:@"LLVM.heap-flag"];

  return (flag && [flag boolValue]);
}

-(Value *) functionCellForSymbol:(id)name
{
  return (Value *) [[self deepPropertyForFunction:name
                          key:@"LLVM.function-cell"]
                     pointerValue];
}

-(Value *) closureDataPointerForSymbol:(id)name
{
  return (Value *) [[self deepPropertyForFunction:name
                          key:@"LLVM.closure-data-pointer"]
                     pointerValue];
}

-(Value *) bindingForSymbol:(id)name
{
  return (Value *) [[self deepPropertyForVariable:name
                          key:@"LLVM.variable-binding"]
                     pointerValue];
}

-(Value *) valueForSymbol:(id)name
{
  return (Value *) [[self deepPropertyForVariable:name
                          key:@"LLVM.variable-value"]
                     pointerValue];
}

-(void) setFunctionCell:(Value *)cellPtr forSymbol:(id)name
{
  [self setDeepProperty:[NSValue valueWithPointer:cellPtr]
        forFunction:name
        key:@"LLVM.function-cell"];
}

-(void) setClosureDataPointer:(Value *)pointer forSymbol:(id)name
{
  [self setDeepProperty:[NSValue valueWithPointer:pointer]
        forFunction:name
        key:@"LLVM.closure-data"];
}

-(void) setBinding:(Value *)binding forSymbol:(id)name
{
  [self setDeepProperty:[NSValue valueWithPointer:binding]
        forVariable:name
        key:@"LLVM.variable-binding"];
}

-(void) setValue:(Value *)value forSymbol:(id)name
{
  [self setDeepProperty:[NSValue valueWithPointer:value]
        forVariable:name
        key:@"LLVM.variable-value"];
}
@end
