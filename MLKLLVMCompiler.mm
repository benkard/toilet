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
#import "globals.h"

#import <Foundation/NSArray.h>
#import <Foundation/NSEnumerator.h>
#import <Foundation/NSString.h>

#include <llvm/Analysis/Verifier.h>
#include <llvm/BasicBlock.h>
#include <llvm/DerivedTypes.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/Module.h>
#include <llvm/ModuleProvider.h>
#include <llvm/PassManager.h>
#include <llvm/Support/IRBuilder.h>
#include <llvm/Value.h>

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
  FunctionType *function_type = FunctionType::get (PointerType::get (Type::Int8Ty, 0),
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
                                 forCompiler:self]];

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
{
  return [form processForLLVM];
}

+(Value *) insertMethodCall:(NSString *)messageName
                   onObject:(Value *)object
         withArgumentVector:(std::vector<Value*> *)argv
{
#ifdef __NEXT_RUNTIME__
#else
#endif
}

+(Value *) insertMethodCall:(NSString *)messageName
                   onObject:(Value *)object
{
  std::vector<Value*> argv;
  return [self insertMethodCall:messageName
               onObject:object
               withArgumentVector:&argv];
}

+(Value *) insertFindClass:(NSString *)className
{
}
@end


@implementation MLKForm (MLKLLVMCompilation)
-(Value *) processForLLVM
{
  NSLog (@"WARNING: Unrecognised form type: %@", self);
  return NULL;
}
@end


@implementation MLKProgNForm (MLKLLVMCompilation)
-(Value *) processForLLVM
{
  NSEnumerator *e = [_bodyForms objectEnumerator];
  MLKForm *form;
  Value *value;

  if ([_bodyForms count] == 0)
    value = ConstantPointerNull::get (PointerType::get(Type::Int8Ty, 0));

  while ((form = [e nextObject]))
    {
      value = [form processForLLVM];
    }
  
  return value;
}
@end


@implementation MLKSimpleLoopForm (MLKLLVMCompilation)
-(Value *) processForLLVM
{
  NSEnumerator *e = [_bodyForms objectEnumerator];
  MLKForm *form;

  Function *function = builder.GetInsertBlock()->getParent();

  BasicBlock *loopBlock = BasicBlock::Create ("loop", function);
  BasicBlock *joinBlock = BasicBlock::Create ("after_loop");

  builder.CreateBr (loopBlock);
  builder.SetInsertPoint (loopBlock);

  while ((form = [e nextObject]))
    {
      [form processForLLVM];
    }

  builder.CreateBr (loopBlock);
  builder.SetInsertPoint (joinBlock);
  function->getBasicBlockList().push_back (joinBlock);

  builder.CreateUnreachable ();

  return NULL;
}
@end


@implementation MLKSymbolForm (MLKLLVMCompilation)
-(Value *) processForLLVM
{
  Value *value;

  if ([_context isHeapVariable:self])
    {
      Value *binding = builder.CreateLoad ([_context bindingForSymbol:_form]);
      value = [_compiler insertMethodCall:@"value" onObject:binding];
    }
  else
    {
      value = builder.CreateLoad ([_context valueForSymbol:_form],
                                  [MLKPrintToString(_form) UTF8String]);
    }

  return value;
}
@end


@implementation MLKFunctionCallForm (MLKLLVMCompilation)
-(Value *) processForLLVM
{
  if (![_context symbolNamesFunction:_head])
    {
      NSLog (@"Compiler: Don't know function %@", MLKPrintToString(_head));
      // XXX Issue a style warning.
    }

  Value *functionCell = builder.CreateLoad ([_context functionCellForSymbol:_head]);
  Value *functionPtr = builder.CreateLoad (functionCell);
  Value *closureDataPointer = builder.CreateLoad ([_context closureDataPointerForSymbol:_head]);

  NSEnumerator *e = [_argumentForms objectEnumerator];
  MLKForm *form;

  std::vector<Value *> args;
  args.push_back (closureDataPointer);

  while ((form = [e nextObject]))
    {
      args.push_back ([form processForLLVM]);
    }

  Value *endmarker = builder.CreateIntToPtr (ConstantInt::get(Type::Int64Ty,
                                                              (uint64_t)MLKEndOfArgumentsMarker,
                                                              false),
  args.push_back (endmarker);

  CallInst *call = builder.CreateCall (functionPtr,
                                       args.begin(),
                                       args.end(),
                                       [MLKPrintToString(_head) UTF8String]);

  return call;
}
@end


@implementation MLKSimpleLambdaForm (MLKLLVMCompilation)
-(Value *) processForLLVM
{
  std::vector <const Type *> argtypes (1, PointerType::get(Type::Int8Ty, 0));
  FunctionType *ftype = FunctionType::get (PointerType::get(Type::Int8Ty, 0),
                                           argtypes,
                                           true);
  Function *function = Function::Create (ftype,
                                         Function::ExternalLinkage,
                                         "",
                                         module);
  BasicBlock *initBlock = BasicBlock::Create ("init_function", function);
  BasicBlock *loopBlock = BasicBlock::Create ("load_args");
  BasicBlock *loopInitBlock = BasicBlock::Create ("load_args_init");
  BasicBlock *joinBlock = BasicBlock::Create ("after_load_args");

  builder.SetInsertPoint (initBlock);

  Value *endmarker = builder.CreateIntToPtr (ConstantInt::get(Type::Int64Ty,
                                                              (uint64_t)MLKEndOfArgumentsMarker,
                                                              false),
                                             PointerType::get(Type::Int8Ty, 0));

  Value *ap = builder.CreateAlloca (Type::Int8Ty);

  Value *nsmutablearray = [_compiler insertFindClass:@"NSMutableArray"];
  Value *mlkcons = [_compiler insertFindClass:@"MLKCons"];
  Value *lambdaList = builder.CreateAlloca (PointerType::get (Type::Int8Ty, 0));

  builder.CreateStore ([_compiler insertMethodCall:@"array"
                                  onObject:nsmutablearray],
                       lambdaList);

  builder.CreateBr (loopInitBlock);
  builder.SetInsertPoint (loopInitBlock);
  function->getBasicBlockList().push_back (loopInitBlock);

  Value *arg = builder.CreateVAArg (ap, PointerType::get(Type::Int8Ty, 0));
  Value *cond = builder.CreateICmpEQ (arg, endmarker);
  builder.CreateCondBr (cond, joinBlock, loopBlock);
  builder.SetInsertPoint (loopBlock);
  function->getBasicBlockList().push_back (loopBlock);

  std::vector <Value *> argv (1, arg);
  builder.CreateStore ([_compiler insertMethodCall:@"addObject:"
                                  onObject:builder.CreateLoad(lambdaList)
                                  withArgumentVector:&argv],
                       lambdaList);

  builder.CreateBr (loopInitBlock);
  builder.SetInsertPoint (joinBlock);
  function->getBasicBlockList().push_back (joinBlock);

  argv[0] = builder.CreateLoad(lambdaList);
  builder.CreateStore ([_compiler insertMethodCall:@"listWithArray:"
                                  onObject:mlkcons
                                  withArgumentVector:&argv],
                       lambdaList);

  verifyFunction (*function);
  fpm->run (*function);
  return function;
}
@end
