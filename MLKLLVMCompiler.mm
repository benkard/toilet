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

#import "MLKDynamicContext.h"
#import "MLKLLVMCompiler.h"
#import "MLKPackage.h"
#import "globals.h"
#import "util.h"

#import <Foundation/NSArray.h>
#import <Foundation/NSAutoreleasePool.h>
#import <Foundation/NSEnumerator.h>
#import <Foundation/NSString.h>

#include <llvm/Analysis/Verifier.h>
#include <llvm/BasicBlock.h>
#include <llvm/CallingConv.h>
#include <llvm/DerivedTypes.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/Instructions.h>
//#include <llvm/Interpreter.h>
#include <llvm/Module.h>
#include <llvm/ModuleProvider.h>
#include <llvm/PassManager.h>
#include <llvm/Support/IRBuilder.h>
#include <llvm/Target/TargetData.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/Transforms/Utils/Cloning.h>  // InlineFunction
#include <llvm/Transforms/Utils/UnifyFunctionExitNodes.h>
#include <llvm/Value.h>

#include <deque>
#include <vector>

using namespace llvm;


static ExecutionEngine *execution_engine;
static llvm::Module *module;
static IRBuilder<true, ConstantFolder> builder;
static FunctionPassManager *fpm;
static PointerType *PointerTy;
static ModuleProvider *module_provider;


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
+(void) load
{
  if (!MLKDefaultCompiler)
    {
      MLKDefaultCompiler = self;
      MLKLoadCompilesP = YES;
    }

  // GNU ld optimises the MLKLLVMCompilation category on
  // MLKLexicalContext away unless we do this.  Man, the crappiness of
  // this Unix stuff is amazing...
  MLKDummyUseLLVMLexicalContext = nil;
}

+(void) initialize
{
  module = new llvm::Module ("MLKLLVMModule");
  execution_engine = ExecutionEngine::create (module);
  PointerTy = PointerType::get(Type::Int8Ty, 0);
  module_provider = new ExistingModuleProvider (module);
  fpm = new FunctionPassManager (module_provider);
  fpm->add (new TargetData (*execution_engine->getTargetData()));
  //fpm->add (new TargetData (module));
  fpm->add (createScalarReplAggregatesPass());
  fpm->add (createInstructionCombiningPass());
  fpm->add (createReassociatePass());
  fpm->add (createGVNPass());
  //  fpm->add (createVerifierPass());
  //fpm->add (createLowerSetJmpPass());
  //fpm->add (createRaiseAllocationsPass());
  fpm->add (createCFGSimplificationPass());
  fpm->add (createPromoteMemoryToRegisterPass());
  //fpm->add (createGlobalOptimizerPass());
  //fpm->add (createGlobalDCEPass());
  //fpm->add (createFunctionInliningPass());

  // Utilities.
  //  fpm->add (createUnifyFunctionExitNodesPass());
}

+(id) compile:(id)object
    inContext:(MLKLexicalContext *)context
{
  NSAutoreleasePool *pool;
  pool = [[NSAutoreleasePool alloc] init];

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
  MLKForm *form = [MLKForm formWithObject:object
                           inContext:context
                           forCompiler:self];

  block = BasicBlock::Create ("entry", function);
  builder.SetInsertPoint (block);

  v = [self processForm:form];

  builder.CreateRet (v);
  verifyFunction (*function);
  fpm->run (*function);

  //function->dump();

  //module->dump();
  //NSLog (@"%p", fn);

  [pool release];
  //NSLog (@"Code compiled.");

#if 1
  // JIT-compile.
  fn = (id (*)()) execution_engine->getPointerToFunction (function);
  // Execute.
  lambdaForm = fn();
  execution_engine->freeMachineCodeForFunction (function);
#else
  Interpreter *i = Interpreter::create (module_provider);
  lambdaForm = i->runFunction (function)->PointerVal;
#endif

  //NSLog (@"Closure built.");

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

+(id) eval:(id)object
{
  return [self compile:object
               inContext:[MLKLexicalContext globalContext]];
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

+(Value *) insertVoidMethodCall:(NSString *)messageName
                       onObject:(Value *)object
             withArgumentVector:(std::vector<Value*> *)argv
{
  return [self insertMethodCall:messageName
               onObject:object
               withArgumentVector:argv
               name:@""
               returnType:(Type::VoidTy)];
}

+(Value *) insertMethodCall:(NSString *)messageName
                   onObject:(Value *)object
         withArgumentVector:(std::vector<Value*> *)argv
                       name:(NSString *)name
{
  return [self insertMethodCall:messageName
               onObject:object
               withArgumentVector:argv
               name:@""
               returnType:PointerTy];
}

+(Value *) insertMethodCall:(NSString *)messageName
                   onObject:(Value *)object
         withArgumentVector:(std::vector<Value*> *)argv
                       name:(NSString *)name
                 returnType:(const Type *)returnType
{
  std::vector <const Type *> argtypes (2, PointerTy);
  FunctionType *ftype = FunctionType::get (returnType, argtypes, true);

  Value *sel = [self insertSelectorLookup:messageName];

#ifdef __NEXT_RUNTIME__
  Constant *function = 
    module->getOrInsertFunction ("objc_msgSend", ftype);
#else
  std::vector <const Type *> lookup_argtypes (2, PointerTy);
  FunctionType *lookup_ftype = FunctionType::get (PointerType::get (ftype, 0),
                                                  lookup_argtypes,
                                                  false);
  Constant *lookup_function = 
    module->getOrInsertFunction ("objc_msg_lookup", lookup_ftype);
  Value *function =
    builder.CreateCall2 (lookup_function, object, sel, "method_impl");
#endif

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

+(void) insertTrace:(NSString *)message
{
  Constant *function =
    module->getOrInsertFunction ("puts",
                                 Type::Int32Ty,
                                 PointerTy,
                                 NULL);

  builder.CreateCall (function, createGlobalStringPtr ([message UTF8String]));
}

+(void) insertPointerTrace:(Value *)pointerValue
{
  Constant *function =
    module->getOrInsertFunction ("printf",
                                 Type::Int32Ty,
                                 PointerTy,
                                 PointerTy,
                                 NULL);

  builder.CreateCall2 (function,
                       createGlobalStringPtr ("%p\n"),
                       builder.CreateBitCast (pointerValue, PointerTy));
}
@end


@implementation MLKForm (MLKLLVMCompilation)
-(Value *) processForLLVM
{
  //[_compiler insertTrace:
  //             [NSString stringWithFormat:
  //                         @"Executing: %@", MLKPrintToString(_form)]];

  Value *result = [self reallyProcessForLLVM];

  //[_compiler insertTrace:
  //             [NSString stringWithFormat:
  //                         @"Done: %@", MLKPrintToString(_form)]];
  return result;
}

-(Value *) reallyProcessForLLVM
{
  NSLog (@"WARNING: Unrecognised form type: %@", self);
  return NULL;
}
@end


@implementation MLKProgNForm (MLKLLVMCompilation)
-(Value *) reallyProcessForLLVM
{
  NSEnumerator *e = [_bodyForms objectEnumerator];
  MLKForm *form;
  Value *value = ConstantPointerNull::get (PointerTy);

  while ((form = [e nextObject]))
    {
      value = [form processForLLVM];
    }

  return value;
}
@end


@implementation MLKSimpleLoopForm (MLKLLVMCompilation)
-(Value *) reallyProcessForLLVM
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
-(Value *) reallyProcessForLLVM
{
  Value *value;

  //NSLog (@"Symbol: %@", MLKPrintToString (_form));
  //[_compiler insertTrace:[NSString stringWithFormat:@"Symbol: %@", _form]];

  if (![_context variableIsLexical:_form])
    {
      //[_compiler insertTrace:@"Dynamic."];
      Value *mlkdynamiccontext = [_compiler insertClassLookup:@"MLKCons"];
      Value *dynctx = [_compiler insertMethodCall:@"currentContext"
                                 onObject:mlkdynamiccontext];

      LRETAIN (_form);  // FIXME: release
      Value *symbolV = builder.CreateIntToPtr (ConstantInt::get(Type::Int64Ty,
                                                                (uint64_t)_form,
                                                                false),
                                               PointerTy);

      std::vector<Value *> args (1, symbolV);
      value = [_compiler insertMethodCall:@"valueForSymbol:"
                         onObject:dynctx
                         withArgumentVector:&args];
    }
  else if ([_context variableHeapAllocationForSymbol:_form])
    {
      //[_compiler insertTrace:@"Global."];
      Value *binding = builder.CreateLoad (builder.Insert ([_context bindingCellValueForSymbol:_form]));
      value = [_compiler insertMethodCall:@"value" onObject:binding];
    }
  else
    {
      value = builder.CreateLoad ([_context valueValueForSymbol:_form],
                                  [MLKPrintToString(_form) UTF8String]);
    }

  return value;
}
@end


@implementation MLKFunctionCallForm (MLKLLVMCompilation)
-(Value *) reallyProcessForLLVM
{
  static MLKPackage *sys = [MLKPackage findPackage:@"TOILET-SYSTEM"];

  BOOL special_dispatch = NO;
  Value *functionCell;
  Value *functionPtr;
  Value *closureDataCell;
  Value *closureDataPtr;
  std::vector<Value *> args;

  if (![_context symbolNamesFunction:_head])
    {
      if (_head && [_head homePackage] == sys)
        {
          special_dispatch = YES;
        }
      else
        {
          NSLog (@"Compiler: Don't know function %@", MLKPrintToString(_head));
          // XXX Issue a style warning.
        }
    }

  if (!special_dispatch)
    {
      functionCell = builder.Insert ([_context functionCellValueForSymbol:_head]);
      functionPtr = builder.CreateLoad (functionCell);
      closureDataCell = builder.Insert ([_context closureDataPointerValueForSymbol:_head]);
      closureDataPtr = builder.CreateLoad (closureDataCell);

      args.push_back (closureDataPtr);
    }
  else
    {
      std::vector<const Type *> argtypes (1, PointerTy);
      functionPtr = builder.CreateIntToPtr (ConstantInt::get(Type::Int64Ty,
                                                             (uint64_t)MLKDispatchRootFunction,
                                                             false),
                                            PointerType::get (FunctionType::get (PointerTy,
                                                                                 argtypes,
                                                                                 true),
                                                              0));
      LRETAIN (_head); // FIXME: release sometime?  On the other hand,
                       // these symbols will probably never be
                       // deallocated anyway.
      args.push_back (builder.CreateIntToPtr (ConstantInt::get(Type::Int64Ty,
                                                               (uint64_t)_head,
                                                               false),
                                              PointerTy));
    }

  NSEnumerator *e = [_argumentForms objectEnumerator];
  MLKForm *form;

  while ((form = [e nextObject]))
    {
      args.push_back ([form processForLLVM]);
    }

  //GlobalVariable *endmarker = module->getGlobalVariable ("MLKEndOfArgumentsMarker", false);
  //endmarker->setConstant (true);
  //GlobalVariable *endmarker = new GlobalVariable (PointerTy, true, GlobalValue::ExternalWeakLinkage);
  Value *endmarker = builder.CreateIntToPtr (ConstantInt::get(Type::Int64Ty,
                                                              (uint64_t)MLKEndOfArgumentsMarker,
                                                              false),
                                             PointerTy);
  args.push_back (endmarker);

  CallInst *call = builder.CreateCall (functionPtr,
                                       args.begin(),
                                       args.end(),
                                       [MLKPrintToString(_head) UTF8String]);

  // XXX
  if (NO && [_context functionInline:_head])
    {
      InlineFunction (call);
    }

  //[_compiler insertTrace:[NSString stringWithFormat:@"%@ done.", MLKPrintToString(_head)]];

  return call;
}
@end


@implementation MLKSimpleLambdaForm (MLKLLVMCompilation)
-(Value *) reallyProcessForLLVM
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
  BasicBlock *lambdaListNewBlock = BasicBlock::Create ("lambda_list_new");
  BasicBlock *lambdaListUpdateBlock = BasicBlock::Create ("lambda_list_update");

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

  Value *mlkcons = [_compiler insertClassLookup:@"MLKCons"];

  // FIXME: Heap-allocate if appropriate.
  Value *lambdaList = builder.CreateAlloca (PointerTy, NULL, "lambda_list");
  Value *lambdaListTail = builder.CreateAlloca (PointerTy, NULL, "lambda_list_tail");

  builder.CreateStore (ConstantPointerNull::get (PointerTy), lambdaList);
  builder.CreateStore (ConstantPointerNull::get (PointerTy), lambdaListTail);

  builder.CreateBr (loopInitBlock);
  builder.SetInsertPoint (loopInitBlock);
  function->getBasicBlockList().push_back (loopInitBlock);

  Value *arg = builder.CreateVAArg (ap, PointerTy, "arg");
  Value *cond = builder.CreateICmpEQ (arg, endmarker);
  builder.CreateCondBr (cond, joinBlock, loopBlock);
  builder.SetInsertPoint (loopBlock);
  function->getBasicBlockList().push_back (loopBlock);

  builder.CreateCondBr (builder.CreateICmpEQ (builder.CreateLoad (lambdaList),
                                              ConstantPointerNull::get (PointerTy)),
                        lambdaListNewBlock,
                        lambdaListUpdateBlock);

  builder.SetInsertPoint (lambdaListNewBlock);
  function->getBasicBlockList().push_back (lambdaListNewBlock);
  std::vector <Value *> argv (1, arg);
  argv.push_back (ConstantPointerNull::get (PointerTy));
  Value *newLambdaList = [_compiler insertMethodCall:@"cons:with:"
                                    onObject:mlkcons
                                    withArgumentVector:&argv];
  builder.CreateStore (newLambdaList, lambdaList);
  builder.CreateStore (newLambdaList, lambdaListTail);
  builder.CreateBr (loopInitBlock);

  builder.SetInsertPoint (lambdaListUpdateBlock);
  function->getBasicBlockList().push_back (lambdaListUpdateBlock);

  Value *newCons = [_compiler insertMethodCall:@"cons:with:"
                              onObject:mlkcons
                              withArgumentVector:&argv];
  std::vector <Value *> setcdr_argv (1, newCons);
  [_compiler insertVoidMethodCall:@"setCdr:"
             onObject:builder.CreateLoad(lambdaListTail)
             withArgumentVector:&setcdr_argv];
  builder.CreateStore (newCons, lambdaListTail);
  builder.CreateBr (loopInitBlock);

  builder.SetInsertPoint (joinBlock);
  function->getBasicBlockList().push_back (joinBlock);

  builder.CreateCall (module->getOrInsertFunction ("llvm.va_end",
                                                   Type::VoidTy,
                                                   PointerTy,
                                                   NULL),
                      ap);

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
      [form->_context setValueValue:lambdaList forSymbol:_lambdaListName];
      value = [form processForLLVM];
    }

  builder.CreateRet (value);

  //function->dump();
  //NSLog (@"Verify...");
  verifyFunction (*function);
  //NSLog (@"Optimise...");
  fpm->run (*function);
  //NSLog (@"Assemble...");
  // Explicit assembly is needed in order to allow libffi to call
  // the function.
  execution_engine->getPointerToFunction (function);
  //NSLog (@"Done.");
  //function->dump();
  //NSLog (@"Function built.");

  builder.SetInsertPoint (outerBlock);

  Value *closure_data = ConstantPointerNull::get (PointerTy);

  argv[0] = function;
  argv[1] = closure_data;
  argv.push_back (builder.CreateIntToPtr (ConstantInt::get(Type::Int64Ty,
                                                           0,
                                                           false),
                                          PointerTy));
  Value *mlkcompiledclosure = [_compiler
                                insertClassLookup:@"MLKCompiledClosure"];
  Value *closure =
    [_compiler insertMethodCall:@"closureWithCode:data:length:"
               onObject:mlkcompiledclosure
               withArgumentVector:&argv];

  //function->viewCFG();

  return closure;
}
@end


@implementation MLKLetForm (MLKLLVMCompilation)
-(Value *) reallyProcessForLLVM
{
  NSEnumerator *e = [_variableBindingForms objectEnumerator];
  Value *value = ConstantPointerNull::get (PointerTy);
  MLKForm *form;
  MLKVariableBindingForm *binding_form;

  while ((binding_form = [e nextObject]))
    {
      // FIXME: Handle heap allocation.
      Value *binding_value = [[binding_form valueForm] processForLLVM];
      Value *binding_variable = builder.CreateAlloca (PointerTy,
                                                      NULL,
                                                      [(MLKPrintToString([binding_form name]))
                                                        UTF8String]);
      builder.CreateStore (binding_value, binding_variable);
      [_bodyContext setValueValue:binding_variable
                    forSymbol:[binding_form name]];
    }

  e = [_bodyForms objectEnumerator];
  while ((form = [e nextObject]))
    {
      value = [form processForLLVM];
    }

  return value;
}
@end


@implementation MLKQuoteForm (MLKLLVMCompilation)
-(Value *) reallyProcessForLLVM
{
  // FIXME: When to release _quotedData?  At the same time the code is
  // released, probably...
  LRETAIN (_quotedData);
  return builder.CreateIntToPtr (ConstantInt::get(Type::Int64Ty,
                                                  (uint64_t)_quotedData,
                                                  false),
                                 PointerTy);
}
@end


@implementation MLKSelfEvaluatingForm (MLKLLVMCompilation)
-(Value *) reallyProcessForLLVM
{
  // FIXME: When to release _form?  At the same time the code is
  // released, probably...
  LRETAIN (_form);
  return builder.CreateIntToPtr (ConstantInt::get(Type::Int64Ty,
                                                  (uint64_t)_form,
                                                  false),
                                 PointerTy);
}
@end


@implementation MLKIfForm (MLKLLVMCompilation)
-(Value *) reallyProcessForLLVM
{
  Function *function = builder.GetInsertBlock()->getParent();
  BasicBlock *thenBlock = BasicBlock::Create ("if_then", function);
  BasicBlock *elseBlock = BasicBlock::Create ("if_else");
  BasicBlock *joinBlock = BasicBlock::Create ("if_join");

  Value *test = builder.CreateICmpNE ([_conditionForm processForLLVM],
                                      ConstantPointerNull::get (PointerTy));
  Value *value = builder.CreateAlloca (PointerTy, NULL, "if_result");
  builder.CreateCondBr (test, thenBlock, elseBlock);

  builder.SetInsertPoint (thenBlock);
  builder.CreateStore ([_consequentForm processForLLVM], value);
  builder.CreateBr (joinBlock);

  builder.SetInsertPoint (elseBlock);
  function->getBasicBlockList().push_back (elseBlock);
  builder.CreateStore ([_alternativeForm processForLLVM], value);
  builder.CreateBr (joinBlock);

  builder.SetInsertPoint (joinBlock);
  function->getBasicBlockList().push_back (joinBlock);

  return builder.CreateLoad (value);
}
@end


@implementation MLKSetQForm (MLKLLVMCompilation)
-(Value *) reallyProcessForLLVM
{
  NSEnumerator *var_e, *value_e;
  MLKForm *valueForm;
  Value *value = ConstantPointerNull::get (PointerTy);
  id variable;

  var_e = [_variables objectEnumerator];
  value_e = [_valueForms objectEnumerator];
  while ((valueForm = [value_e nextObject]))
    {
      variable = [var_e nextObject];
      value = [valueForm processForLLVM];
      if (![_context variableIsLexical:variable])
        {
          Value *mlkdynamiccontext = [_compiler insertClassLookup:@"MLKCons"];
          Value *dynctx = [_compiler insertMethodCall:@"currentContext"
                                     onObject:mlkdynamiccontext];

          LRETAIN (variable);  // FIXME: release
          Value *symbolV = builder.CreateIntToPtr (ConstantInt::get(Type::Int64Ty,
                                                                    (uint64_t)variable,
                                                                    false),
                                                   PointerTy);

          std::vector<Value *> args;
          args.push_back (value);
          args.push_back (symbolV);
          [_compiler insertMethodCall:@"setValue:forSymbol:"
                     onObject:dynctx
                     withArgumentVector:&args];          
        }
      else if ([_context variableHeapAllocationForSymbol:variable])
        {
          Value *binding = builder.CreateLoad (builder.Insert ([_context
                                                                 bindingCellValueForSymbol:variable]));
          std::vector<Value *> args (1, value);

          [_compiler insertVoidMethodCall:@"setValue:"
                     onObject:binding
                     withArgumentVector:&args];
        }
      else
        {
          builder.CreateStore (value, [_context valueValueForSymbol:variable]);
        }
    }

  return value;
}
@end


@implementation MLKInPackageForm (MLKLLVMCompilation)
-(Value *) reallyProcessForLLVM
{
  id package = [MLKPackage findPackage:stringify(_packageDesignator)];

  [[MLKDynamicContext currentContext]
    setValue:package
    forSymbol:[[MLKPackage findPackage:@"COMMON-LISP"]
                intern:@"*PACKAGE*"]];

  return builder.CreateIntToPtr (ConstantInt::get(Type::Int64Ty,
                                                  (uint64_t)package,
                                                  false),
                                 PointerTy);
}
@end
