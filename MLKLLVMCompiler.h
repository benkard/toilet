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

#import "MLKForm.h"
#import "MLKInterpreter.h"
#import "MLKLexicalContext.h"
#import "MLKLexicalContext-MLKLLVMCompilation.h"

#import <Foundation/NSObject.h>
#import <Foundation/NSString.h>

#ifdef __cplusplus
#include <vector>
#include <llvm/Value.h>
#include <llvm/BasicBlock.h>
using namespace llvm;
#endif

@interface MLKLLVMCompiler : NSObject
+(void) load;

+(void) initialize;

+(id) compile:(id)object
    inContext:(MLKLexicalContext *)context;

+(id) eval:(id)object;

+(void) processTopLevelForm:(id)object;
+(void) processTopLevelForm:(id)object
                     inMode:(enum MLKProcessingMode)mode;

#ifdef __cplusplus
+(Value *) processForm:(MLKForm *)form;

+(Value *) insertSelectorLookup:(NSString *)name;

+(Value *) insertMethodCall:(NSString *)messageName
                   onObject:(Value *)object
         withArgumentVector:(std::vector<Value*> *)argv;
+(Value *) insertMethodCall:(NSString *)messageName
                   onObject:(Value *)object
         withArgumentVector:(std::vector<Value*> *)argv
                       name:(NSString *)name;
+(Value *) insertMethodCall:(NSString *)messageName
                   onObject:(Value *)object
         withArgumentVector:(std::vector<Value*> *)argv
                       name:(NSString *)name
                 returnType:(const Type *)returnType;
+(Value *) insertVoidMethodCall:(NSString *)messageName
                       onObject:(Value *)object
             withArgumentVector:(std::vector<Value*> *)argv;

+(Value *) insertMethodCall:(NSString *)messageName
                   onObject:(Value *)object;
+(Value *) insertMethodCall:(NSString *)messageName
                   onObject:(Value *)object
                   withName:(NSString *)name;

+(Value *) insertClassLookup:(NSString *)className;

+(void) insertTrace:(NSString *)message;
#endif
@end


#ifdef __cplusplus
@interface MLKForm (MLKLLVMCompilation)
-(Value *) processForLLVM;
@end
#endif
