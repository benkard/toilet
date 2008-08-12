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
static PointerType *PointerTy;


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
  id lambdaForm;
  id (*fn)();

  block = BasicBlock::Create ("entry", function);
  builder.SetInsertPoint (block);

  v = [self processForm:[MLKForm formWithObject:object
                                 inContext:context
                                 forCompiler:self]];

  builder.CreateRet (v);
  function->dump();
  verifyFunction (*function);
  fpm->run (*function);

  // JIT-compile.
  fn = (id (*)()) execution_engine->getPointerToFunction (function);

  // Execute.
  lambdaForm = fn();

  return lambdaForm;
}

+(void) processTopLevelForm:(id)object
{
  [self processTopLevelForm:object
        inMode:not_compile_time_mode];
}


+(void) processTopLevelForm:(id)object
                     inMode:(enum MLKProcessingMode)mode
{
  //FIXME
  // If PROGN, do this...  If EVAL-WHEN, do that...

  
}

+(Value *) processForm:(MLKForm *)form
{
  return [form processForLLVM];
}

+(Value *) insertSelectorLookup:(NSString *)name
{
  Constant *function = 
    module->getOrInsertFunction (
#ifdef __NEXT_RUNTIME__
                                 "sel_getUid",
#else
                                 "sel_get_uid",
#endif
                                 PointerTy,
                                 PointerTy,
                                 NULL);

  Constant *nameptr = createGlobalStringPtr ([name UTF8String]);
  return builder.CreateCall (function, nameptr, "selector");
}

+(Value *) insertMethodCall:(NSString *)messageName
                   onObject:(Value *)object
         withArgumentVector:(std::vector<Value*> *)argv
{
  return [self insertMethodCall:messageName
               onObject:object
               withArgumentVector:argv
               name:@""];
}

+(Value *) insertMethodCall:(NSString *)messageName
                   onObject:(Value *)object
         withArgumentVector:(std::vector<Value*> *)argv
                       name:(NSString *)name;
{
  std::vector <const Type *> argtypes (2, PointerTy);
  FunctionType *ftype = FunctionType::get (PointerTy, argtypes, true);
  Constant *function = 
    module->getOrInsertFunction (
#ifdef __NEXT_RUNTIME__
                                 "objc_msgSend",
#else
                                 "objc_msg_send",
#endif
                                 ftype);

  Value *sel = [self insertSelectorLookup:messageName];

  // XXX The following doesn't work.  Why?
  //  std::deque <Value *> argd (*argv);
  //  argd.push_front (sel);
  //  argd.push_front (object);

  std::vector <Value *> argd;
  argd.push_back (object);
  argd.push_back (sel);
  std::vector<Value *>::iterator e;
  for (e = argv->begin(); e != argv->end(); e++)
    argd.push_back (*e);

  return builder.CreateCall (function, argd.begin(), argd.end());
}

+(Value *) insertMethodCall:(NSString *)messageName
                   onObject:(Value *)object
                   withName:(NSString *)name
{
  std::vector<Value*> argv;
  return [self insertMethodCall:messageName
               onObject:object
               withArgumentVector:&argv
               name:name];
}

+(Value *) insertMethodCall:(NSString *)messageName
                   onObject:(Value *)object
{
  return [self insertMethodCall:messageName
               onObject:object
               withName:@""];
}

+(Value *) insertClassLookup:(NSString *)className
{
  Constant *function = 
    module->getOrInsertFunction (
#ifdef __NEXT_RUNTIME__
                                 "objc_getClass",
#else
                                 "objc_get_class",
#endif
                                 PointerTy,
                                 PointerTy,
                                 NULL);

  const char *cname = [className UTF8String];

  // Value *nameptr = builder.CreateGlobalStringPtr (cname, "");
  Constant *nameptr = createGlobalStringPtr (cname);
  return builder.CreateCall (function, nameptr, cname);
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
    value = ConstantPointerNull::get (PointerTy);

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
                                         Function::InternalLinkage,
                                         "",
                                         module);

  Function::arg_iterator args = function->arg_begin();
  Value *closure_data_arg = args++;
  closure_data_arg->setName ("closure_data");

  BasicBlock *outerBlock = builder.GetInsertBlock ();
  BasicBlock *initBlock = BasicBlock::Create ("init_function", function);
  BasicBlock *loopBlock = BasicBlock::Create ("load_args");
  BasicBlock *loopInitBlock = BasicBlock::Create ("load_args_prelude");
  BasicBlock *joinBlock = BasicBlock::Create ("function_body");

  builder.SetInsertPoint (initBlock);

  Value *endmarker = builder.CreateIntToPtr (ConstantInt::get(Type::Int64Ty,
                                                              (uint64_t)MLKEndOfArgumentsMarker,
                                                              false),
                                             PointerType::get(Type::Int8Ty, 0));

  Value *ap = builder.CreateAlloca (Type::Int8Ty, NULL, "ap");

  builder.CreateCall (module->getOrInsertFunction ("llvm.va_start",
                                                   Type::VoidTy,
                                                   PointerTy,
                                                   NULL),
                      ap);

  Value *nsmutablearray = [_compiler insertClassLookup:@"NSMutableArray"];
  Value *mlkcons = [_compiler insertClassLookup:@"MLKCons"];
  Value *lambdaList = builder.CreateAlloca (PointerTy, NULL, "lambda_list");

  builder.CreateStore ([_compiler insertMethodCall:@"array"
                                  onObject:nsmutablearray],
                       lambdaList);

  builder.CreateBr (loopInitBlock);
  builder.SetInsertPoint (loopInitBlock);
  function->getBasicBlockList().push_back (loopInitBlock);

  Value *arg = builder.CreateVAArg (ap, PointerTy, "arg");
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

  builder.CreateCall (module->getOrInsertFunction ("llvm.va_end",
                                                   Type::VoidTy,
                                                   PointerTy,
                                                   NULL),
                      ap);

  argv[0] = builder.CreateLoad(lambdaList);
  builder.CreateStore ([_compiler insertMethodCall:@"listWithArray:"
                                  onObject:mlkcons
                                  withArgumentVector:&argv],
                       lambdaList);

  NSEnumerator *e = [_bodyForms objectEnumerator];
  MLKForm *form;
  Value *value = NULL;

  if ([_bodyForms count] == 0)
    {
      //NSLog (@"%LAMBDA: No body.");
      value = ConstantPointerNull::get (PointerTy);
    }

  while ((form = [e nextObject]))
    {
      //NSLog (@"%LAMBDA: Processing subform.");
      value = [form processForLLVM];
    }

  builder.CreateRet (value);

  function->dump();
  NSLog (@"Verify...");
  verifyFunction (*function);
  NSLog (@"FPM...");
  fpm->run (*function);

  builder.SetInsertPoint (outerBlock);

  Value *closure_data = ConstantPointerNull::get (PointerTy);

  argv[0] = function;
  argv.push_back (closure_data);
  Value *mlkcompiledclosure = [_compiler
                                insertClassLookup:@"MLKCompiledClosure"];
  Value *closure =
    [_compiler insertMethodCall:@"closureWithCode:data:"
               onObject:mlkcompiledclosure
               withArgumentVector:&argv];
  outerBlock->dump();

  return closure;
}
@end
