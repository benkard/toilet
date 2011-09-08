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

extern llvm::LLVMContext* llvm_context;
extern const llvm::Type* Int8Ty;
extern const llvm::Type* Int16Ty;
extern const llvm::Type* Int32Ty;
extern const llvm::Type* Int64Ty;
extern const llvm::Type* VoidTy;
extern const llvm::PointerType* VoidPointerTy;
extern const llvm::PointerType* PointerPointerTy;

#endif
