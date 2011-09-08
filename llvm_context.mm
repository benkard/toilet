//
//  llvm_context.cpp
//  Toilet Lisp
//
//  Created by Matthias Benkard on 08.09.11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#define __STDC_CONSTANT_MACROS 1

#include "llvm_context.h"

#include <llvm/DerivedTypes.h>
#include <llvm/LLVMContext.h> 
#include <llvm/Support/TypeBuilder.h>
using namespace llvm;

LLVMContext *llvm_context;
//const Type* IntPtrTy = IntegerType::getInt32Ty(C);
const Type* Int8Ty;
const Type* Int16Ty;
const Type* Int32Ty;
const Type* Int64Ty;
//const Type* VoidTy = TypeBuilder<void, false>::get(llvm_context);
const Type* VoidTy;
const PointerType* VoidPointerTy;
const PointerType* PointerPointerTy;
