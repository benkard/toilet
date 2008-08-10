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

#import "MLKLLVMCompiler.h"

#import <Foundation/NSArray.h>
#import <Foundation/NSString.h>

#include <Analysis/Verifier.h>
#include <BasicBlock.h>
#include <DerivedTypes.h>
#include <ExecutionEngine/ExecutionEngine.h>
#include <Module.h>
#include <ModuleProvider.h>
#include <PassManager.h>
#include <Support/IRBuilder.h>
#include <Value.h>

using namespace llvm;


static ExecutionEngine *execution_engine;
static llvm::Module *module;
static IRBuilder builder;
static FunctionPassManager *fpm;


@implementation MLKLLVMCompiler
+(void) initialize
{
  module = new llvm::Module ("MLKLLVMModule");
  execution_engine = ExecutionEngine::create (module);
}

+(id) compile:(id)object
    inContext:(MLKLexicalContext *)context
{
  Value *v = NULL;
  BasicBlock *block;
  std::vector<const Type*> noargs (0, Type::VoidTy);
  FunctionType *function_type = FunctionType::get (PointerType::get (Type::VoidTy, 0),
                                                   noargs,
                                                   false);
  Function *function = Function::Create (function_type,
                                         Function::ExternalLinkage,
                                         "",
                                         module);
  id (*fn)();

  block = BasicBlock::Create ("entry", function);
  builder.SetInsertPoint (block);

  v = [self processForm:[MLKForm formWithObject:object
                                 inContext:context
                                 forCompiler:self]
            inBlock:&block];

  builder.CreateRet (v);
  verifyFunction (*function);
  fpm->run (*function);

  // JIT-compile.
  fn = (id (*)()) execution_engine->getPointerToFunction (function);
  return fn ();
}

+(void) processTopLevelForm:(id)object
{
  //FIXME
}

+(Value *) processForm:(MLKForm *)form
               inBlock:(BasicBlock **)block
{
  return [form processForLLVMInBlock:block];
}
@end


@implementation MLKForm (MLKLLVMCompilation)
-(Value *) processForLLVMInBlock:(BasicBlock **)block
{
  NSLog (@"WARNING: Unrecognised form type: %@", self);
  return NULL;
}
@end
