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
#include <llvm/BasicBlock.h>
#include <llvm/Constants.h>
#include <llvm/DerivedTypes.h>
#include <llvm/Instructions.h>
#include <llvm/Value.h>
using namespace llvm;
using namespace std;


id MLKDummyUseLLVMLexicalContext = nil;

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
  id flag = [self propertyForVariable:name
                  key:@"LLVM.heap-flag"];

  if (flag)
    return [flag boolValue];
  else
    return [self variableIsGlobal:name];
}

-(Instruction *) functionCellValueForSymbol:(id)name
{
  std::vector<const Type *> types (1, PointerType::get(PointerType::get(Type::Int8Ty, 0), 0));
  return (new IntToPtrInst (ConstantInt::get(Type::Int64Ty,
                                             (uint64_t)[self functionCellForSymbol:name],
                                             false),
                            PointerType::get(PointerType::get(FunctionType::get(PointerType::get(Type::Int8Ty,
                                                                                                 0),
                                                                                types,
                                                                                true),
                                                              0),
                                             0)));
}

-(Instruction *) closureDataPointerValueForSymbol:(id)name
{
  return (new IntToPtrInst (ConstantInt::get(Type::Int64Ty,
                                             (uint64_t)[self closureDataPointerForSymbol:name],
                                             false),
                            PointerType::get(PointerType::get(PointerType::get(Type::Int8Ty, 0), 0), 0)));
}

-(Instruction *) closureDataLengthValueForSymbol:(id)name
{
  // The length cell isn't really a void** but an intptr_t*.
  return (new IntToPtrInst (ConstantInt::get(Type::Int64Ty,
                                             (uint64_t)[self closureDataLengthForSymbol:name],
                                             false),
                            PointerType::get(PointerType::get(Type::Int8Ty, 0), 0)));
}

-(Instruction *) globalBindingValueForSymbol:(id)name
{
  return (new IntToPtrInst (ConstantInt::get(Type::Int64Ty,
                                             (uint64_t)[self bindingForSymbol:name],
                                             false),
                            PointerType::get(Type::Int8Ty, 0)));
}

-(Value *) bindingValueForSymbol:(id)name
{
  return (Value *) [[self propertyForVariable:name
                          key:@"LLVM.variable-binding"]
                     pointerValue];
}

-(void) locallySetBindingValue:(Value *)value forSymbol:(id)name
{
  [self addShallowProperty:[NSValue valueWithPointer:value]
        forVariable:name
        key:@"LLVM.variable-binding"];
}

-(void) setBindingValue:(Value *)value forSymbol:(id)name
{
  [self setDeepProperty:[NSValue valueWithPointer:value]
        forVariable:name
        key:@"LLVM.variable-binding"];
}

-(Value *) valueValueForSymbol:(id)name
{
  return (Value *) [[self propertyForVariable:name
                          key:@"LLVM.variable-value"]
                     pointerValue];
}

-(Value *) functionBindingValueForSymbol:(id)name
{
  return (Value *) [[self propertyForVariable:name
                                          key:@"LLVM.function-binding"]
                    pointerValue];
}

-(void) locallySetFunctionBindingValue:(Value *)value forSymbol:(id)name
{
  [self addShallowProperty:[NSValue valueWithPointer:value]
               forVariable:name
                       key:@"LLVM.function-binding"];
}

-(void) setFunctionBindingValue:(Value *)value forSymbol:(id)name
{
  [self setDeepProperty:[NSValue valueWithPointer:value]
            forVariable:name
                    key:@"LLVM.function-binding"];
}

// -(void) setFunctionCellValue:(Value *)cellPtr forSymbol:(id)name
// {
//   [self setDeepProperty:[NSValue valueWithPointer:cellPtr]
//         forFunction:name
//         key:@"LLVM.function-cell"];
// }

// -(void) setClosureDataPointerValue:(Value *)pointer forSymbol:(id)name
// {
//   [self setDeepProperty:[NSValue valueWithPointer:pointer]
//         forFunction:name
//         key:@"LLVM.closure-data"];
// }

// -(void) setBindingValue:(Value *)binding forSymbol:(id)name
// {
//   [self setDeepProperty:[NSValue valueWithPointer:binding]
//         forVariable:name
//         key:@"LLVM.variable-binding"];
// }

-(void) setValueValue:(Value *)value forSymbol:(id)name
{
  [self setDeepProperty:[NSValue valueWithPointer:value]
        forVariable:name
        key:@"LLVM.variable-value"];
}
@end
