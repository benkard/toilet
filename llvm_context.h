//
//  llvm_context.h
//  Toilet Lisp
//
//  Created by Matthias Benkard on 08.09.11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#ifndef Toilet_Lisp_llvm_context_h
#define Toilet_Lisp_llvm_context_h

#include <llvm/DerivedTypes.h>
#include <llvm/LLVMContext.h> 
#include <llvm/Support/TypeBuilder.h>

LLVMContext llvm_context;
//const Type* IntPtrTy = IntegerType::getInt32Ty(C);
const Type* Int8Ty = IntegerType::getInt8Ty(llvm_context);
const Type* Int16Ty = IntegerType::getInt16Ty(llvm_context);
const Type* Int32Ty = IntegerType::getInt32Ty(llvm_context);
const Type* Int64Ty = IntegerType::getInt64Ty(llvm_context);
//const Type* VoidTy = TypeBuilder<void, false>::get(llvm_context);
const Type* VoidTy = Type::getVoidTy(llvm_context);
const PointerType* VoidPointerTy = PointerType::get(Int8Ty, 0);
const PointerType* PointerPointerTy = PointerType::get(VoidPointerTy, 0);

#endif
