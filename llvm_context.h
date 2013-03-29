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
extern llvm::Type* Int8Ty;
extern llvm::Type* Int16Ty;
extern llvm::Type* Int32Ty;
extern llvm::Type* Int64Ty;
extern llvm::Type* VoidTy;
extern llvm::PointerType* VoidPointerTy;
extern llvm::PointerType* PointerPointerTy;

#endif
