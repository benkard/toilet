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
#include <llvm/CallingConv.h>
#include <llvm/DerivedTypes.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/Instructions.h>
#include <llvm/Module.h>
#include <llvm/ModuleProvider.h>
#include <llvm/PassManager.h>
#include <llvm/Support/IRBuilder.h>
#include <llvm/Value.h>

#include <deque>
#include <vector>

using namespace llvm;


static ExecutionEngine *execution_engine;
static llvm::Module *module;
static IRBuilder builder;
static FunctionPassManager *fpm;
static Type *PointerTy;
static Function *f_sel_get_uid;
static Function *f_objc_msg_send;
static Function *f_objc_get_class;


static Constant
*createGlobalStringPtr (const char *string)
{
  Constant *(indices[2]);
  indices[0] = indices[1] = ConstantInt::get (Type::Int32Ty, 0);

  Constant *str = ConstantArray::get (string);
  Constant *str2 = new GlobalVariable (str->getType(),
                                       true, 
                                       GlobalValue::InternalLinkage,
                                       str,
                                       "",
                                       module);
  Constant *ptr = ConstantExpr::getGetElementPtr (str2, indices, 2);
  return ptr;
}


@implementation MLKLLVMCompiler
+(void) initialize
{
  module = new llvm::Module ("MLKLLVMModule");
  execution_engine = ExecutionEngine::create (module);
  PointerTy = PointerType::get(Type::Int8Ty, 0);

  std::vector<const Type*> argtypes1 (1, PointerTy);
  FunctionType* ftype = FunctionType::get (PointerTy, argtypes1, false);
  f_sel_get_uid = Function::Create(ftype,
                                   GlobalValue::ExternalLinkage,
#ifdef __NEXT_RUNTIME__
                                   "sel_getUid",
#else
                                   "sel_get_uid",
#endif
                                   module);
  //sel_get_uid->setCallingConv (CallingConv::C);


  std::vector<const Type*> argtypes2 (2, PointerTy);
  ftype = FunctionType::get (PointerTy, argtypes2, true);
  f_objc_msg_send = Function::Create(ftype,
                                     GlobalValue::ExternalLinkage,
#ifdef __NEXT_RUNTIME__
                                     "objc_msgSend",
#else
                                     "objc_msg_send",
#endif
                                     module);


#if 0
  std::vector<const Type*> argtypes3 (1, PointerTy);
  ftype = FunctionType::get (PointerTy, argtypes3, false);
  f_objc_get_class = Function::Create(ftype,
                                      GlobalValue::ExternalLinkage,
#ifdef __NEXT_RUNTIME__
                                      "objc_getClass",
#else
                                      "objc_get_class",
#endif
                                      module);
  f_objc_get_class->setCallingConv (CallingConv::C);

  f_objc_get_class->dump();
#endif
}

+(id) compile:(id)object
    inContext:(MLKLexicalContext *)context
{
  Value *v = NULL;
  BasicBlock *block;
  std::vector<const Type*> noargs (0, Type::VoidTy);
  FunctionType *function_type = FunctionType::get (PointerTy,
                                                   noargs,
                                                   false);
  Function *function = Function::Create (function_type,
                                         Function::ExternalLinkage,
                                         "",
                                         module);
  id (*fn)();

  block = BasicBlock::Create ("entry", function);
  builder.SetInsertPoint (block);

  function->dump();

  v = [self processForm:[MLKForm formWithObject:object
                                 inContext:context
                                 forCompiler:self]];

  builder.CreateRet (v);
  function->dump();
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

+(Value *) insertSelectorLookup:(NSString *)name
{
  llvm::Constant *nameval = ConstantArray::get ([name UTF8String]);
  return builder.CreateCall (f_sel_get_uid, nameval);
}

+(Value *) insertMethodCall:(NSString *)messageName
                   onObject:(Value *)object
         withArgumentVector:(std::vector<Value*> *)argv
{
  Value *sel = [self insertSelectorLookup:messageName];

  std::deque <Value *> argd (argv->begin(), argv->end());
  argd.push_front (object);
  argd.push_front (sel);
  return builder.CreateCall (f_objc_msg_send, argd.begin(), argd.end());
}

+(Value *) insertMethodCall:(NSString *)messageName
                   onObject:(Value *)object
{
  std::vector<Value*> argv;
  return [self insertMethodCall:messageName
               onObject:object
               withArgumentVector:&argv];
}

+(Value *) insertClassLookup:(NSString *)className
{
//   Constant *nameval = ConstantArray::get (std::string ([className UTF8String]));
//   GlobalVariable *namevar = new GlobalVariable (nameval->getType(),  //ArrayType::get(IntegerType::get (8), 4),
//                                                 true,
//                                                 GlobalValue::InternalLinkage,
//                                                 nameval,
//                                                 ".blargh");
//  Value *ptr = builder.CreateGEP (namevar, Constant::getNullValue(IntegerType::get(32)));

//  nameval->dump();
//  namevar->dump();

  Constant *function = 
    module->getOrInsertFunction ("objc_get_class",
                                 PointerTy,
                                 PointerTy,
                                 NULL);

  //  Value *nameptr = builder.CreateGlobalStringPtr ([className UTF8String], ".blargh");
  Constant *nameptr = createGlobalStringPtr ([className UTF8String]);
  nameptr->dump();
  builder.GetInsertBlock()->getParent()->dump();
  return builder.CreateCall (function, nameptr);
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
  Value *value = NULL;

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
                                             PointerTy);
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
  std::vector <const Type *> argtypes (1, PointerTy);
  FunctionType *ftype = FunctionType::get (PointerTy, argtypes, true);
  Function *function = Function::Create (ftype,
                                         Function::ExternalLinkage,
                                         "",
                                         module);
  function->dump();

  BasicBlock *initBlock = BasicBlock::Create ("init_function", function);
  BasicBlock *loopBlock = BasicBlock::Create ("load_args");
  BasicBlock *loopInitBlock = BasicBlock::Create ("load_args_init");
  BasicBlock *joinBlock = BasicBlock::Create ("after_load_args");

  function->dump();

  builder.SetInsertPoint (initBlock);

  Value *endmarker = builder.CreateIntToPtr (ConstantInt::get(Type::Int64Ty,
                                                              (uint64_t)MLKEndOfArgumentsMarker,
                                                              false),
                                             PointerType::get(Type::Int8Ty, 0));

  function->dump();

  Value *ap = builder.CreateAlloca (Type::Int8Ty);

  function->dump();

  Value *nsmutablearray = [_compiler insertClassLookup:@"NSMutableArray"];
  function->dump();
  Value *mlkcons = [_compiler insertClassLookup:@"MLKCons"];
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

  function->dump();
  verifyFunction (*function);
  fpm->run (*function);
  return function;
}
@end
