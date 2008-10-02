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

#import "MLKCompiledClosure.h"
#import "MLKDynamicContext.h"
#import "MLKLLVMCompiler.h"
#import "MLKPackage.h"
#import "globals.h"
#import "util.h"

#import <Foundation/NSArray.h>
#import <Foundation/NSAutoreleasePool.h>
#import <Foundation/NSEnumerator.h>
#import <Foundation/NSString.h>

#ifdef __OBJC_GC__
#import <Foundation/NSGarbageCollector.h>
#endif


#include <llvm/Analysis/Verifier.h>
#include <llvm/BasicBlock.h>
#include <llvm/CallingConv.h>
#include <llvm/DerivedTypes.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/GenericValue.h>
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

#include <stddef.h>
#ifdef MACOSX
#include <objc/objc-api.h>
#if defined(OBJC_API_VERSION) && OBJC_API_VERSION >= 2
#include <objc/runtime.h>
#endif
#endif

using namespace llvm;
using namespace std;


static ExecutionEngine *execution_engine;
static llvm::Module *module;
#if defined(LLVM_MAJOR_VERSION) && (LLVM_MAJOR_VERSION <= 2) && (LLVM_MINOR_VERSION <= 3)
static IRBuilder builder;
#else
static IRBuilder<true, ConstantFolder> builder;
#endif
static FunctionPassManager *fpm;
static PointerType *VoidPointerTy, *PointerPointerTy;
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
  module_provider = new ExistingModuleProvider (module);

  //execution_engine = ExecutionEngine::create (module_provider, true);
  execution_engine = ExecutionEngine::create (module_provider, false);

  VoidPointerTy = PointerType::get(Type::Int8Ty, 0);
  PointerPointerTy = PointerType::get(VoidPointerTy, 0);

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
  vector<const Type*> noargs (0, Type::VoidTy);
  FunctionType *function_type = FunctionType::get (VoidPointerTy,
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
  [self markVariablesForHeapAllocationInForm:form];

  block = BasicBlock::Create ("entry", function);
  builder.SetInsertPoint (block);

  v = [self processForm:form];

  builder.CreateRet (v);
  verifyFunction (*function);
  fpm->run (*function);

  //function->dump();

  //module->dump();
  //NSLog (@"%p", fn);

  LRELEASE (pool);
  //NSLog (@"Code compiled.");

#if 1
  // JIT-compile.
  vector<GenericValue> nogenericargs;
  lambdaForm = (id)execution_engine->runFunction (function, nogenericargs).PointerVal;
  //fn = (id (*)()) execution_engine->getPointerToFunction (function);
  // Execute.
  //lambdaForm = fn();
  // FIXME: Free machine code when appropriate.  (I.e. now?  But this crashes after a LOAD.)
  //execution_engine->freeMachineCodeForFunction (function);
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
  return [form processForLLVMWithMultiValue:NULL];
}

+(void) markVariablesForHeapAllocationInForm:(MLKForm *)form
{
  NSArray *subforms = [form subforms];
  unsigned int i;

  //NSLog (@"Marking %@.", form);
  for (i = 0; i < [subforms count]; i++)
    {
      MLKForm *subform = [subforms objectAtIndex:i];

      [self markVariablesForHeapAllocationInForm:subform];

      if ([subform isKindOfClass:[MLKSimpleLambdaForm class]]
          || [subform isKindOfClass:[MLKLambdaForm class]])
        {
          NSArray *freeVariables = [[subform freeVariables] allObjects];
          unsigned int j;

          for (j = 0; j < [freeVariables count]; j++)
            {
              id variable = [freeVariables objectAtIndex:j];
              [[subform context] setVariableHeapAllocation:YES
                                 forSymbol:variable];
            }
        }
    }
  //NSLog (@"%@ marked.", form);
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
                                 VoidPointerTy,
                                 VoidPointerTy,
                                 NULL);

  Constant *nameptr = createGlobalStringPtr ([name UTF8String]);
  return builder.CreateCall (function, nameptr, "selector");
}

+(Value *) insertMethodCall:(NSString *)messageName
                   onObject:(Value *)object
         withArgumentVector:(vector<Value*> *)argv
{
  return [self insertMethodCall:messageName
               onObject:object
               withArgumentVector:argv
               name:@""];
}

+(Value *) insertVoidMethodCall:(NSString *)messageName
                       onObject:(Value *)object
             withArgumentVector:(vector<Value*> *)argv
{
  return [self insertMethodCall:messageName
               onObject:object
               withArgumentVector:argv
               name:@""
               returnType:(Type::VoidTy)];
}

+(Value *) insertMethodCall:(NSString *)messageName
                   onObject:(Value *)object
         withArgumentVector:(vector<Value*> *)argv
                       name:(NSString *)name
{
  return [self insertMethodCall:messageName
               onObject:object
               withArgumentVector:argv
               name:@""
               returnType:VoidPointerTy];
}

+(Value *) insertMethodCall:(NSString *)messageName
                   onObject:(Value *)object
         withArgumentVector:(vector<Value*> *)argv
                       name:(NSString *)name
                 returnType:(const Type *)returnType
{
  vector <const Type *> argtypes (2, VoidPointerTy);
  FunctionType *ftype = FunctionType::get (returnType, argtypes, true);

  Value *sel = [self insertSelectorLookup:messageName];

#ifdef __NEXT_RUNTIME__
  Constant *function = 
    module->getOrInsertFunction ("objc_msgSend", ftype);
#else
  vector <const Type *> lookup_argtypes (2, VoidPointerTy);
  FunctionType *lookup_ftype = FunctionType::get (PointerType::get (ftype, 0),
                                                  lookup_argtypes,
                                                  false);
  Constant *lookup_function = 
    module->getOrInsertFunction ("objc_msg_lookup", lookup_ftype);
  Value *function =
    builder.CreateCall2 (lookup_function, object, sel, "method_impl");
#endif

  // XXX The following doesn't work.  Why?
  //  deque <Value *> argd (*argv);
  //  argd.push_front (sel);
  //  argd.push_front (object);

  vector <Value *> argd;
  argd.push_back (object);
  argd.push_back (sel);
  vector<Value *>::iterator e;
  for (e = argv->begin(); e != argv->end(); e++)
    argd.push_back (*e);

  return builder.CreateCall (function, argd.begin(), argd.end());
}

+(Value *) insertMethodCall:(NSString *)messageName
                   onObject:(Value *)object
                   withName:(NSString *)name
{
  vector<Value*> argv;
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
                                 VoidPointerTy,
                                 VoidPointerTy,
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
                                 VoidPointerTy,
                                 NULL);

  builder.CreateCall (function, createGlobalStringPtr ([message UTF8String]));
}

+(void) insertPointerTrace:(Value *)pointerValue
{
  Constant *function =
    module->getOrInsertFunction ("printf",
                                 Type::Int32Ty,
                                 VoidPointerTy,
                                 VoidPointerTy,
                                 NULL);

  builder.CreateCall2 (function,
                       createGlobalStringPtr ("%p\n"),
                       builder.CreateBitCast (pointerValue, VoidPointerTy));
}
@end


@implementation MLKForm (MLKLLVMCompilation)
-(Value *) processForLLVMWithMultiValue:(Value *)multiValue
{
#if 0
  [_compiler insertTrace:
               [NSString stringWithFormat:
                           @"Executing: %@", MLKPrintToString(_form)]];
#endif

  Value *result = [self reallyProcessForLLVMWithMultiValue:multiValue];

#if 0
  [_compiler insertTrace:
               [NSString stringWithFormat:
                           @"Done: %@", MLKPrintToString(_form)]];
#endif

  return result;
}

-(Value *) reallyProcessForLLVMWithMultiValue:(Value *)multiValue
{
  NSLog (@"WARNING: Unrecognised form type: %@", self);
  return NULL;
}
@end


@implementation MLKProgNForm (MLKLLVMCompilation)
-(Value *) reallyProcessForLLVMWithMultiValue:(Value *)multiValue
{
  NSEnumerator *e = [_bodyForms objectEnumerator];
  MLKForm *form;
  Value *value = ConstantPointerNull::get (VoidPointerTy);
  int i;

  i = 0;
  while ((form = [e nextObject]))
    {
      i++;
      if (i == [_bodyForms count])
        value = [form processForLLVMWithMultiValue:multiValue];
      else
        value = [form processForLLVMWithMultiValue:NULL];
    }

  return value;
}
@end


@implementation MLKSimpleLoopForm (MLKLLVMCompilation)
-(Value *) reallyProcessForLLVMWithMultiValue:(Value *)multiValue
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
      [form processForLLVMWithMultiValue:NULL];
    }

  builder.CreateBr (loopBlock);
  builder.SetInsertPoint (joinBlock);
  function->getBasicBlockList().push_back (joinBlock);

  builder.CreateUnreachable ();

  return ConstantPointerNull::get (VoidPointerTy);;
}
@end


@implementation MLKSymbolForm (MLKLLVMCompilation)
-(Value *) reallyProcessForLLVMWithMultiValue:(Value *)multiValue
{
  Value *value;

  //NSLog (@"Symbol: %@", MLKPrintToString (_form));
  //[_compiler insertTrace:[NSString stringWithFormat:@"Symbol: %@", _form]];

  if (![_context variableIsLexical:_form])
    {
      //[_compiler insertTrace:@"Dynamic."];
      Value *mlkdynamiccontext = [_compiler insertClassLookup:@"MLKDynamicContext"];
      Value *dynctx = [_compiler insertMethodCall:@"currentContext"
                                 onObject:mlkdynamiccontext];

      LRETAIN (_form);  // FIXME: release
      Value *symbolV = builder.CreateIntToPtr (ConstantInt::get(Type::Int64Ty,
                                                                (uint64_t)_form,
                                                                false),
                                               VoidPointerTy);

      vector<Value *> args (1, symbolV);
      value = [_compiler insertMethodCall:@"valueForSymbol:"
                         onObject:dynctx
                         withArgumentVector:&args];
    }
  else if ([_context variableIsGlobal:_form])
    {
      //[_compiler insertTrace:@"Global."];
      Value *binding = builder.Insert ([_context globalBindingValueForSymbol:_form]);
      value = [_compiler insertMethodCall:@"value" onObject:binding];      
    }
  else if ([_context variableHeapAllocationForSymbol:_form])
    {
      Value *binding = [_context bindingValueForSymbol:_form];
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
-(Value *) reallyProcessForLLVMWithMultiValue:(Value *)multiValue
{
  Value *functionPtr;
  Value *closureDataPtr;
  vector<Value *> args;

  if (![_context symbolNamesFunction:_head])
    {
      NSLog (@"Compiler: Don't know function %@", MLKPrintToString(_head));
      // XXX Issue a style warning.
    }

  if ([_context functionIsGlobal:_head])
    {
      Value *functionCell;
      Value *closureDataCell;

      functionCell = builder.Insert ([_context functionCellValueForSymbol:_head]);
      functionPtr = builder.CreateLoad (functionCell);
      closureDataCell = builder.Insert ([_context closureDataPointerValueForSymbol:_head]);
      closureDataPtr = builder.CreateLoad (closureDataCell);
    }
  else
    {
      Value *binding = [_context functionBindingValueForSymbol:_head];
      // It's important for closure to be an i8* because we need to calculate
      // the GEP offset in terms of bytes.
      Value *closure = builder.CreateBitCast ([_compiler insertMethodCall:@"value" onObject:binding], VoidPointerTy);

#if defined(OBJC_API_VERSION) && OBJC_API_VERSION >= 2
      ptrdiff_t code_offset = ivar_getOffset (class_getInstanceVariable ([MLKCompiledClosure class], "m_code"));
      ptrdiff_t data_offset = ivar_getOffset (class_getInstanceVariable ([MLKCompiledClosure class], "m_data"));
#else
      ptrdiff_t code_offset = offsetof (MLKCompiledClosure, m_code);
      ptrdiff_t data_offset = offsetof (MLKCompiledClosure, m_data);
#endif
      Constant *code_offset_value = ConstantInt::get (Type::Int32Ty, code_offset, false);
      Constant *data_offset_value = ConstantInt::get (Type::Int32Ty, data_offset, false);
      Value *codeptr = builder.CreateGEP (closure, code_offset_value);
      Value *dataptr = builder.CreateGEP (closure, data_offset_value);
      codeptr = builder.CreateBitCast (codeptr, PointerPointerTy, "closure_code_ptr");
      dataptr = builder.CreateBitCast (codeptr, PointerPointerTy, "closure_data_ptr");
      Value *code = builder.CreateLoad (codeptr, "closure_code");
      Value *data = builder.CreateLoad (dataptr, "closure_data");

      std::vector<const Type *> types (2, PointerPointerTy);
      functionPtr = builder.CreateBitCast (code, PointerType::get(FunctionType::get(VoidPointerTy,
                                                                                    types,
                                                                                    true),
                                                                  0));
      closureDataPtr = builder.CreateBitCast (data, PointerPointerTy);
    }

  //[_compiler insertTrace:[NSString stringWithFormat:@"Call: %@", MLKPrintToString(_head)]];
  //[_compiler insertPointerTrace:functionPtr];

  args.push_back (closureDataPtr);
  if (multiValue)
    args.push_back (multiValue);
  else
    args.push_back (ConstantPointerNull::get (PointerPointerTy));

  NSEnumerator *e = [_argumentForms objectEnumerator];
  MLKForm *form;

  while ((form = [e nextObject]))
    {
      args.push_back ([form processForLLVMWithMultiValue:NULL]);
    }

  //GlobalVariable *endmarker = module->getGlobalVariable ("MLKEndOfArgumentsMarker", false);
  //endmarker->setConstant (true);
  //GlobalVariable *endmarker = new GlobalVariable (VoidPointerTy, true, GlobalValue::ExternalWeakLinkage);
  Value *endmarker = builder.CreateIntToPtr (ConstantInt::get(Type::Int64Ty,
                                                              (uint64_t)MLKEndOfArgumentsMarker,
                                                              false),
                                             VoidPointerTy);
  args.push_back (endmarker);

  // If the pointer output here is different from the one above,
  // there's some stack smashing going on.
  //[_compiler insertTrace:[NSString stringWithFormat:@"Now calling: %@.", MLKPrintToString(_head)]];
  //[_compiler insertPointerTrace:functionPtr];

  CallInst *call = builder.CreateCall (functionPtr,
                                       args.begin(),
                                       args.end(),
                                       [MLKPrintToString(_head) UTF8String]);
  call->setCallingConv(CallingConv::C);
  call->setTailCall(true);

  // XXX
  if ([_context functionIsInline:_head])
    {
      InlineFunction (call);
    }

  //[_compiler insertTrace:[NSString stringWithFormat:@"%@ done.", MLKPrintToString(_head)]];

  return call;
}
@end


static void
build_simple_function_definition (MLKBodyForm *processed_form,
                                  id _lambdaListName,
                                  Function*& function,
                                  Value*& closure_data,
                                  intptr_t& closure_data_size)
{
  NSArray *_bodyForms = [processed_form bodyForms];
  MLKLexicalContext *_bodyContext = [processed_form bodyContext];
  MLKLexicalContext *_context = [processed_form context];
  id _compiler = [MLKLLVMCompiler class];

  vector <const Type *> argtypes (2, PointerPointerTy);
  FunctionType *ftype = FunctionType::get (VoidPointerTy, argtypes, true);
  function = Function::Create (ftype,
                               Function::InternalLinkage,
                               "a_lisp_closure_body",
                               module);

  Function::arg_iterator args = function->arg_begin();
  Value *closure_data_arg = args++;
  closure_data_arg->setName ("closure_data");
  
  Value *functionMultiValue = args++;
  functionMultiValue->setName ("function_multiple_value_return_pointer");

  BasicBlock *outerBlock = builder.GetInsertBlock ();
  BasicBlock *initBlock = BasicBlock::Create ("init_function", function);
  BasicBlock *loopBlock = BasicBlock::Create ("load_args");
  BasicBlock *loopInitBlock = BasicBlock::Create ("load_args_prelude");
  BasicBlock *joinBlock = BasicBlock::Create ("function_body");
  BasicBlock *lambdaListNewBlock = BasicBlock::Create ("lambda_list_new");
  BasicBlock *lambdaListUpdateBlock = BasicBlock::Create ("lambda_list_update");

  // ***** HANDLE CLOSURE VARIABLES *****
  builder.SetInsertPoint (outerBlock);

  NSArray *freeVariables = [[processed_form freeVariables] allObjects];
  closure_data = builder.CreateAlloca (VoidPointerTy,
                                       ConstantInt::get(Type::Int32Ty,
                                                        (uint32_t)[freeVariables count],
                                                        false));
  closure_data_size = 0;
  unsigned int i;
  for (i = 0; i < [freeVariables count]; i++)
    {
      // FIXME: We assume heap allocation for all closure variables.
      MLKSymbol *symbol = [freeVariables objectAtIndex:i];
      if (![_context variableIsGlobal:symbol])
        {
          Constant *position = ConstantInt::get(Type::Int32Ty, closure_data_size, false);

          // Fill in the closure data array.
          builder.SetInsertPoint (outerBlock);
          Value *binding = [_context bindingValueForSymbol:symbol];
          Value *closure_value_ptr = builder.CreateGEP (closure_data, position);
          builder.CreateStore (binding, closure_value_ptr);

          // Access the closure data array from within the closure.
          builder.SetInsertPoint (initBlock);
          Value *local_closure_value_ptr = builder.CreateGEP (closure_data_arg,
                                                              position);
          Value *local_closure_value = builder.CreateLoad (local_closure_value_ptr,
                                                           [MLKPrintToString(symbol) UTF8String]);
          [_bodyContext locallySetBindingValue:local_closure_value
                        forSymbol:symbol];

          closure_data_size++;
        }
    }


  // ***** HANDLE ARGUMENTS *****
  builder.SetInsertPoint (initBlock);
  Value *endmarker = builder.CreateIntToPtr (ConstantInt::get(Type::Int64Ty,
                                                              (uint64_t)MLKEndOfArgumentsMarker,
                                                              false),
                                             PointerType::get(Type::Int8Ty, 0));

  Value *ap = builder.CreateAlloca (VoidPointerTy, NULL, "ap");
  Value *ap2 = builder.CreateBitCast (ap, VoidPointerTy);

  builder.CreateCall (module->getOrInsertFunction ("llvm.va_start",
                                                   Type::VoidTy,
                                                   VoidPointerTy,
                                                   NULL),
                      ap2);

  Value *mlkcons = [_compiler insertClassLookup:@"MLKCons"];

  // FIXME: Heap-allocate if appropriate.
  Value *lambdaList = builder.CreateAlloca (VoidPointerTy, NULL, "lambda_list");
  Value *lambdaListTail = builder.CreateAlloca (VoidPointerTy, NULL, "lambda_list_tail");

  builder.CreateStore (ConstantPointerNull::get (VoidPointerTy), lambdaList);
  builder.CreateStore (ConstantPointerNull::get (VoidPointerTy), lambdaListTail);

  builder.CreateBr (loopInitBlock);
  builder.SetInsertPoint (loopInitBlock);
  function->getBasicBlockList().push_back (loopInitBlock);

  Value *arg = builder.CreateVAArg (ap, VoidPointerTy, "arg");
  Value *cond = builder.CreateICmpEQ (arg, endmarker);
  builder.CreateCondBr (cond, joinBlock, loopBlock);
  builder.SetInsertPoint (loopBlock);
  function->getBasicBlockList().push_back (loopBlock);

  builder.CreateCondBr (builder.CreateICmpEQ (builder.CreateLoad (lambdaList),
                                              ConstantPointerNull::get (VoidPointerTy)),
                        lambdaListNewBlock,
                        lambdaListUpdateBlock);

  builder.SetInsertPoint (lambdaListNewBlock);
  function->getBasicBlockList().push_back (lambdaListNewBlock);
  vector <Value *> argv (1, arg);
  argv.push_back (ConstantPointerNull::get (VoidPointerTy));
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
  vector <Value *> setcdr_argv (1, newCons);
  [_compiler insertVoidMethodCall:@"setCdr:"
             onObject:builder.CreateLoad(lambdaListTail)
             withArgumentVector:&setcdr_argv];
  builder.CreateStore (newCons, lambdaListTail);
  builder.CreateBr (loopInitBlock);

  builder.SetInsertPoint (joinBlock);
  function->getBasicBlockList().push_back (joinBlock);

  builder.CreateCall (module->getOrInsertFunction ("llvm.va_end",
                                                   Type::VoidTy,
                                                   VoidPointerTy,
                                                   NULL),
                      ap2);

  if ([_bodyContext variableHeapAllocationForSymbol:_lambdaListName])
    {
      Value *mlkbinding = [_compiler insertClassLookup:@"MLKBinding"];
      Value *currentLambdaList = builder.CreateLoad (lambdaList);
      vector<Value *> args (1, currentLambdaList);
      Value *lambdaBinding = [_compiler insertMethodCall:@"bindingWithValue:"
                                        onObject:mlkbinding
                                        withArgumentVector:&args];
      [_bodyContext setBindingValue:lambdaBinding
                    forSymbol:_lambdaListName];
    }
  else
    [_bodyContext setValueValue:lambdaList forSymbol:_lambdaListName];

  NSEnumerator *e = [_bodyForms objectEnumerator];
  MLKForm *form;
  Value *value = NULL;

  if ([_bodyForms count] == 0)
    {
      //NSLog (@"%LAMBDA: No body.");
      value = ConstantPointerNull::get (VoidPointerTy);
    }

  i = 0;
  while ((form = [e nextObject]))
    {
      i++;
      if (i == [_bodyForms count])
        value = [form processForLLVMWithMultiValue:functionMultiValue];
      else
        value = [form processForLLVMWithMultiValue:NULL];
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
  //function->viewCFG();
  //NSLog (@"Function built.");

  builder.SetInsertPoint (outerBlock);
}


@implementation MLKSimpleLambdaForm (MLKLLVMCompilation)
-(Value *) reallyProcessForLLVMWithMultiValue:(Value *)multiValue
{
  intptr_t closure_data_size;
  Function *function;
  Value *closure_data;

  build_simple_function_definition (self, _lambdaListName, function, closure_data, closure_data_size);

  vector<Value *> argv;
  argv.push_back (function);
  argv.push_back (builder.CreateBitCast (closure_data, VoidPointerTy));
  argv.push_back (builder.CreateIntToPtr (ConstantInt::get(Type::Int32Ty,
                                                           closure_data_size,
                                                           false),
                                          VoidPointerTy));
  Value *mlkcompiledclosure = [_compiler
                                insertClassLookup:@"MLKCompiledClosure"];
  Value *closure =
    [_compiler insertMethodCall:@"closureWithCode:data:length:"
               onObject:mlkcompiledclosure
               withArgumentVector:&argv];

  return closure;
}
@end


@implementation MLKLetForm (MLKLLVMCompilation)
-(Value *) reallyProcessForLLVMWithMultiValue:(Value *)multiValue
{
  NSEnumerator *e = [_variableBindingForms objectEnumerator];
  Value *value = ConstantPointerNull::get (VoidPointerTy);
  MLKForm *form;
  MLKVariableBindingForm *binding_form;

  while ((binding_form = [e nextObject]))
    {
      Value *binding_value = [[binding_form valueForm] processForLLVMWithMultiValue:NULL];

      if ([_bodyContext variableHeapAllocationForSymbol:[binding_form name]])
        {
          Value *mlkbinding = [_compiler insertClassLookup:@"MLKBinding"];
          vector<Value *> args (1, binding_value);
          Value *binding = [_compiler insertMethodCall:@"bindingWithValue:"
                                      onObject:mlkbinding
                                      withArgumentVector:&args];
          [_bodyContext setBindingValue:binding
                        forSymbol:[binding_form name]];
        }
      else
        {
          Value *binding_variable = builder.CreateAlloca (VoidPointerTy,
                                                          NULL,
                                                          [(MLKPrintToString([binding_form name]))
                                                            UTF8String]);
          builder.CreateStore (binding_value, binding_variable);

          [_bodyContext setValueValue:binding_variable
                        forSymbol:[binding_form name]];
        }
    }

  int i = 0;
  e = [_bodyForms objectEnumerator];
  while ((form = [e nextObject]))
    {
      i++;
      if (i == [_bodyForms count])
        value = [form processForLLVMWithMultiValue:multiValue];
      else
        value = [form processForLLVMWithMultiValue:NULL];
    }

  return value;
}
@end


@implementation MLKSimpleFletForm (MLKLLVMCompilation)
-(Value *) reallyProcessForLLVMWithMultiValue:(Value *)multiValue
{
  NSEnumerator *e = [_functionBindingForms objectEnumerator];
  Value *value = ConstantPointerNull::get (VoidPointerTy);
  MLKForm *form;
  MLKSimpleFunctionBindingForm *binding_form;
  unsigned int i;
  
  while ((binding_form = [e nextObject]))
    {
      intptr_t closure_data_size;
      Function *function;
      Value *closure_data;
      
      build_simple_function_definition (binding_form, [binding_form lambdaListName], function, closure_data, closure_data_size);
      
      vector<Value *> argv;
      argv.push_back (function);
      argv.push_back (builder.CreateBitCast (closure_data, VoidPointerTy));
      argv.push_back (builder.CreateIntToPtr (ConstantInt::get(Type::Int32Ty,
                                                               closure_data_size,
                                                               false),
                                              VoidPointerTy));
      Value *mlkcompiledclosure = [_compiler
                                   insertClassLookup:@"MLKCompiledClosure"];
      Value *closure =
        [_compiler insertMethodCall:@"closureWithCode:data:length:"
                           onObject:mlkcompiledclosure
                 withArgumentVector:&argv];

      Value *binding_value = closure;

      Value *mlkbinding = [_compiler insertClassLookup:@"MLKBinding"];
      vector<Value *> args (1, binding_value);
      Value *binding = [_compiler insertMethodCall:@"bindingWithValue:"
                                          onObject:mlkbinding
                                withArgumentVector:&args];
      [_bodyContext setFunctionBindingValue:binding
                                  forSymbol:[binding_form name]];
    }

  i = 0;
  e = [_bodyForms objectEnumerator];
  while ((form = [e nextObject]))
    {
      i++;
      if (i == [_bodyForms count])
        value = [form processForLLVMWithMultiValue:multiValue];
      else
        value = [form processForLLVMWithMultiValue:NULL];
    }
  
  return value;
}
@end


@implementation MLKQuoteForm (MLKLLVMCompilation)
-(Value *) reallyProcessForLLVMWithMultiValue:(Value *)multiValue
{
  // FIXME: When to release _quotedData?  At the same time the code is
  // released, probably...
  // FIXME: In garbage-collected code, _quotedData will be deleted even
  // though it is referenced by compiled code!
  LRETAIN (_quotedData);
#ifdef __OBJC_GC__
  if (_quotedData && MLKInstanceP (_quotedData))
    [[NSGarbageCollector defaultCollector] disableCollectorForPointer:_quotedData];
#endif

  return builder.CreateIntToPtr (ConstantInt::get(Type::Int64Ty,
                                                  (uint64_t)_quotedData,
                                                  false),
                                 VoidPointerTy);
}
@end


@implementation MLKSelfEvaluatingForm (MLKLLVMCompilation)
-(Value *) reallyProcessForLLVMWithMultiValue:(Value *)multiValue
{
  // FIXME: When to release _form?  At the same time the code is
  // released, probably...
  // FIXME: In garbage-collected code, _form will be deleted even
  // though it is referenced by compiled code!
  LRETAIN (_form);
#ifdef __OBJC_GC__
  if (_form && MLKInstanceP (_form))
    [[NSGarbageCollector defaultCollector] disableCollectorForPointer:_form];
#endif

  return builder.CreateIntToPtr (ConstantInt::get(Type::Int64Ty,
                                                  (uint64_t)_form,
                                                  false),
                                 VoidPointerTy);
}
@end


@implementation MLKIfForm (MLKLLVMCompilation)
-(Value *) reallyProcessForLLVMWithMultiValue:(Value *)multiValue
{
  Function *function = builder.GetInsertBlock()->getParent();
  BasicBlock *thenBlock = BasicBlock::Create ("if_then", function);
  BasicBlock *elseBlock = BasicBlock::Create ("if_else");
  BasicBlock *joinBlock = BasicBlock::Create ("if_join");

  Value *test = builder.CreateICmpNE ([_conditionForm processForLLVMWithMultiValue:NULL],
                                      ConstantPointerNull::get (VoidPointerTy));
  Value *value = builder.CreateAlloca (VoidPointerTy, NULL, "if_result");
  builder.CreateCondBr (test, thenBlock, elseBlock);

  builder.SetInsertPoint (thenBlock);
  builder.CreateStore ([_consequentForm processForLLVMWithMultiValue:multiValue], value);
  builder.CreateBr (joinBlock);

  builder.SetInsertPoint (elseBlock);
  function->getBasicBlockList().push_back (elseBlock);
  builder.CreateStore ([_alternativeForm processForLLVMWithMultiValue:multiValue], value);
  builder.CreateBr (joinBlock);

  builder.SetInsertPoint (joinBlock);
  function->getBasicBlockList().push_back (joinBlock);

  return builder.CreateLoad (value);
}
@end


@implementation MLKSetQForm (MLKLLVMCompilation)
-(Value *) reallyProcessForLLVMWithMultiValue:(Value *)multiValue
{
  NSEnumerator *var_e, *value_e;
  MLKForm *valueForm;
  Value *value = ConstantPointerNull::get (VoidPointerTy);
  id variable;

  var_e = [_variables objectEnumerator];
  value_e = [_valueForms objectEnumerator];
  while ((valueForm = [value_e nextObject]))
    {
      variable = [var_e nextObject];
      value = [valueForm processForLLVMWithMultiValue:NULL];
      if (![_context variableIsLexical:variable])
        {
          Value *mlkdynamiccontext = [_compiler insertClassLookup:@"MLKDynamicContext"];
          Value *dynctx = [_compiler insertMethodCall:@"currentContext"
                                     onObject:mlkdynamiccontext];

          LRETAIN (variable);  // FIXME: release
#ifdef __OBJC_GC__
          // FIXME: proper memory management
          if (variable && MLKInstanceP (variable))
            [[NSGarbageCollector defaultCollector] disableCollectorForPointer:variable];
#endif

          Value *symbolV = builder.CreateIntToPtr (ConstantInt::get(Type::Int64Ty,
                                                                    (uint64_t)variable,
                                                                    false),
                                                   VoidPointerTy);

          vector<Value *> args;
          args.push_back (symbolV);
          Value *binding = [_compiler insertMethodCall:@"bindingForSymbol:"
                                              onObject:dynctx
                                    withArgumentVector:&args];

          // Test whether the binding is non-null.  If so, set its value, else create a new one.

          Function *function = builder.GetInsertBlock()->getParent();
          BasicBlock *setBlock = BasicBlock::Create ("setq_set_existing_dynamic_binding", function);
          BasicBlock *makeNewBlock = BasicBlock::Create ("setq_make_new_dynamic_binding");
          BasicBlock *joinBlock = BasicBlock::Create ("setq_join");

          Value *test = builder.CreateICmpNE (binding, ConstantPointerNull::get (VoidPointerTy));
          //Value *value = builder.CreateAlloca (VoidPointerTy, NULL, "if_result");
          builder.CreateCondBr (test, setBlock, makeNewBlock);

          builder.SetInsertPoint (setBlock);
          args[0] = value;
          [_compiler insertMethodCall:@"setValue:"
                             onObject:binding
                   withArgumentVector:&args];
          builder.CreateBr (joinBlock);

          builder.SetInsertPoint (makeNewBlock);
          function->getBasicBlockList().push_back (makeNewBlock);
          Value *globalctx = [_compiler insertMethodCall:@"globalContext"
                                                onObject:mlkdynamiccontext];
          args[0] = value;
          args.push_back (symbolV);
          [_compiler insertMethodCall:@"addValue:forSymbol:"
                             onObject:globalctx
                   withArgumentVector:&args];
          builder.CreateBr (joinBlock);

          builder.SetInsertPoint (joinBlock);
          function->getBasicBlockList().push_back (joinBlock);
        }
      else if ([_context variableIsGlobal:variable])
        {
          Value *binding = builder.Insert ([_context globalBindingValueForSymbol:variable]);
          vector<Value *> args (1, value);

          [_compiler insertVoidMethodCall:@"setValue:"
                     onObject:binding
                     withArgumentVector:&args];
        }
      else if ([_context variableHeapAllocationForSymbol:variable])
        {
          Value *binding = [_context bindingValueForSymbol:variable];
          vector<Value *> args (1, value);

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
-(Value *) reallyProcessForLLVMWithMultiValue:(Value *)multiValue
{
  id package = [MLKPackage findPackage:stringify(_packageDesignator)];

  [[MLKDynamicContext currentContext]
    setValue:package
    forSymbol:[[MLKPackage findPackage:@"COMMON-LISP"]
                intern:@"*PACKAGE*"]];

  return builder.CreateIntToPtr (ConstantInt::get(Type::Int64Ty,
                                                  (uint64_t)package,
                                                  false),
                                 VoidPointerTy);
}
@end


@implementation MLKSimpleFunctionForm (MLKLLVMCompilation)
-(Value *) reallyProcessForLLVMWithMultiValue:(Value *)multiValue
{
  if ([_context functionIsGlobal:_functionName])
    {
      Value *mlklexicalenvironment = [_compiler insertClassLookup:@"MLKLexicalEnvironment"];
      Value *env = [_compiler insertMethodCall:@"globalEnvironment"
                                      onObject:mlklexicalenvironment];

      LRETAIN (_functionName);  // FIXME: release
#ifdef __OBJC_GC__
      // FIXME: proper memory management
      if (_functionName && MLKInstanceP (_functionName))
        [[NSGarbageCollector defaultCollector] disableCollectorForPointer:_functionName];
#endif
      
      Value *symbolV = builder.CreateIntToPtr (ConstantInt::get(Type::Int64Ty,
                                                                (uint64_t)_functionName,
                                                                false),
                                               VoidPointerTy);

      vector<Value *> args;
      args.push_back (symbolV);
      Value *fun = [_compiler insertMethodCall:@"functionForSymbol:"
                                      onObject:env
                            withArgumentVector:&args];
      return fun;
    }
  else
    {
      Value *binding = [_context functionBindingValueForSymbol:_functionName];
      Value *closure = builder.CreateBitCast ([_compiler insertMethodCall:@"value" onObject:binding], VoidPointerTy);

      return closure;
    }
}
@end


@implementation MLKLambdaFunctionForm (MLKLLVMCompilation)
-(Value *) reallyProcessForLLVMWithMultiValue:(Value *)multiValue
{
  return [_lambdaForm processForLLVMWithMultiValue:multiValue];
}
@end