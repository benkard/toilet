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

#ifdef __cplusplus
//#define __STDC_CONSTANT_MACROS
#include <vector>
#include <llvm/Value.h>
#include <llvm/BasicBlock.h>
#include <llvm/Instructions.h>
//using namespace llvm;
#endif

#import <Foundation/NSObject.h>
#import <Foundation/NSString.h>


extern id MLKDummyUseLLVMLexicalContext;

@interface MLKLexicalContext (MLKLLVMCompilation)
#ifdef __cplusplus
-(void) setVariableHeapAllocation:(BOOL)heapp forSymbol:(id)name;
-(BOOL) variableHeapAllocationForSymbol:(id)name;
-(llvm::Instruction *) functionCellValueForSymbol:(id)name;
-(llvm::Instruction *) closureDataPointerValueForSymbol:(id)name;
-(llvm::Instruction *) closureDataLengthValueForSymbol:(id)name;
-(llvm::Value *) bindingValueForSymbol:(id)name;
-(void) locallySetBindingValue:(llvm::Value *)value forSymbol:(id)name;
-(void) setBindingValue:(llvm::Value *)value forSymbol:(id)name;
-(llvm::Value *) functionBindingValueForSymbol:(id)name;
-(void) locallySetFunctionBindingValue:(llvm::Value *)value forSymbol:(id)name;
-(void) setFunctionBindingValue:(llvm::Value *)value forSymbol:(id)name;
-(llvm::Instruction *) globalBindingValueForSymbol:(id)name;
-(llvm::Value *) valueValueForSymbol:(id)name;
//-(void) setFunctionCellValue:(llvm::Value *)cellPtr forSymbol:(id)name;
//-(void) setClosureDataPointerValue:(llvm::Value *)pointer forSymbol:(id)name;
//-(void) setBindingValue:(llvm::Value *)binding forSymbol:(id)name;
-(void) setValueValue:(llvm::Value *)value forSymbol:(id)name;
#endif
@end
