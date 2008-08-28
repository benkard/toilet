## -*- mode: makefile-gmake; coding: utf-8 -*-
##
## Toilet Lisp, a Common Lisp subset for the Étoilé runtime.
## Copyright (C) 2008  Matthias Andreas Benkard.
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or (at
## your option) any later version.
##
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.


export USE_LLVM ADDITIONAL_OBJCFLAGS ADDITIONAL_LDFLAGS LLVM_CONFIG

KIT_TARGETS = ToiletKit

USE_LLVM := YES
ifeq ($(USE_LLVM),YES)
KIT_TARGETS += libtoilet-llvm
endif

default: $(KIT_TARGETS) toilet ToiletLisp

include $(GNUSTEP_MAKEFILES)/common.make

#all:: ToiletKit etshell Test

APP_NAME = ToiletLisp
TOOL_NAME = etshell toilet
FRAMEWORK_NAME = ToiletKit
BUNDLE_NAME = Test
LIBRARY_NAME =

ADDITIONAL_OBJCFLAGS += $(CUSTOM_OBJCFLAGS)
ADDITIONAL_LDFLAGS += $(CUSTOM_LDFLAGS)

ifeq ($(DEBUG),YES)
ADDITIONAL_OBJCFLAGS += -ggdb3
endif

# I know, I know.  I'm emulating ‘configure’ here.  *shrug* Whatever.
HAVE_FFI_H := $(shell echo '\#include <ffi.h>' | $(CC) $(ADDITIONAL_OBJCFLAGS) -c -o /dev/null -x c - 2>/dev/null && echo YES)

ifeq ($(HAVE_FFI_H),YES)
  ADDITIONAL_OBJCFLAGS += -DHAVE_FFI_H
else
  HAVE_FFI_FFI_H := $(shell echo '\#include <ffi/ffi.h>' | $(CC) $(ADDITIONAL_OBJCFLAGS) -c -o /dev/null -x c - 2>/dev/null && echo YES)

  ifeq ($(HAVE_FFI_FFI_H),YES)
  ADDITIONAL_OBJCFLAGS += -DHAVE_FFI_FFI_H
  else
    $(error "Could not find ffi.h.  Please install libffi and pass appropriate CUSTOM_OBJCFLAGS and CUSTOM_LDFLAGS to make.")
  endif
endif

ToiletKit_OBJC_FILES = functions.m globals.m MLKArray.m				\
                       MLKBackquoteReader.m MLKBinaryStream.m			\
                       MLKBinaryStreamCharacterStream.m MLKBinding.m		\
                       MLKCharacter.m MLKCharacterStream.m			\
                       MLKCommaReader.m MLKCompiledClosure.m MLKCons.m		\
                       MLKDoubleFloat.m						\
                       MLKDispatchingMacroCharacterReader.m			\
                       MLKDynamicContext.m MLKEnvironment.m			\
                       MLKFileHandleStream.m MLKFloat.m				\
                       MLKForeignProcedure.m MLKForm.m MLKInteger.m		\
                       MLKInterpretedClosure.m MLKInterpreter.m			\
                       MLKLexicalContext.m MLKLexicalEnvironment.m		\
                       MLKNumber.m MLKPackage.m MLKParenReader.m		\
                       MLKQuoteReader.m MLKRatio.m MLKReader.m			\
                       MLKReadtable.m MLKReaderError.m MLKRoot.m		\
                       MLKSemicolonReader.m MLKSharpsignColonReader.m		\
                       MLKSingleFloat.m MLKStreamStream.m			\
                       MLKStringInputStream.m MLKStringOutputStream.m		\
                       MLKStringReader.m MLKSymbol.m MLKThrowException.m	\
                       MLKValuesFunction.m NSObject-MLKPrinting.m		\
                       NSString-MLKPrinting.m
ToiletKit_OBJCFLAGS = -Wall
ToiletKit_LDFLAGS = -lgmp -lffi -ldl
#LIBRARIES_DEPEND_UPON


ifeq ($(USE_LLVM),YES)
LLVM_CONFIG = llvm-config
ADDITIONAL_OBJCFLAGS += -DUSE_LLVM -DLLVM_MAJOR_VERSION=$(shell $(LLVM_CONFIG) --version | cut -f 1 -d.) -DLLVM_MINOR_VERSION=$(shell $(LLVM_CONFIG) --version | cut -f 2 -d. | sed s/svn//)
LLVM_LDFLAGS = $(shell $(LLVM_CONFIG) --ldflags) $(shell $(LLVM_CONFIG) --libs backend interpreter engine linker codegen transformutils scalaropts analysis ipo)
endif

ifeq ($(BUILD_TOILET_LLVM),YES)
ifeq ($(USE_LLVM),YES)
static = yes # This line is the reason for this whole “BUILD_TOILET_LLVM”
             # recursive-make-gone-awry crap.  Hooray for not being able
             # to build static libraries without bending over backwards!
             # Thanks a bunch, GNUstep-Make!

LIBRARY_NAME += libtoilet-llvm

ADDITIONAL_OBJCCFLAGS = $(ADDITIONAL_OBJCFLAGS)
libtoilet-llvm_OBJC_FILES += MLKLexicalContext-MLKLLVMCompilation.m
libtoilet-llvm_OBJCC_FILES = MLKLLVMCompiler.mm
libtoilet-llvm_OBJCFLAGS = -DUSE_LLVM
libtoilet-llvm_OBJCCFLAGS = -DUSE_LLVM `$(LLVM_CONFIG) --cxxflags` $(ToiletKit_OBJCFLAGS)
endif
else #!BUILD_TOILET_LLVM
libtoilet-llvm:
	$(MAKE) $@ shared=no BUILD_TOILET_LLVM=YES
endif

-include GNUmakefile.preamble
include $(GNUSTEP_MAKEFILES)/library.make
-include GNUmakefile.postamble


#TOOL_NAME = etoilet
#etoilet_OBJC_FILES = main.m
#etoilet_OBJC_LIBS = -lToiletKit -LToiletKit.framework

etshell_OBJC_FILES = StepTalkShell/STShell.m		\
                     StepTalkShell/STShell+output.m	\
                     StepTalkShell/stshell_tool.m
etshell_OBJC_LIBS += -lStepTalk -lreadline -lncurses -lToiletKit	\
                     -LToiletKit.framework                              \
                     -LToiletKit.framework/Versions/Current
etshell_OBJCFLAGS = -w

toilet_OBJC_FILES = MLKReadEvalPrintLoop.m
toilet_OBJCC_FILES = _stamp.mm
toilet_OBJC_LIBS += -ledit -lncurses -LToiletKit.framework	\
                    -LToiletKit.framework/Versions/Current -lToiletKit
toilet_OBJCFLAGS = -Wall

ifeq ($(USE_LLVM),YES)
toilet_OBJC_LIBS += -Lobj -ltoilet-llvm $(LLVM_LDFLAGS)
endif


ToiletLisp_OBJC_FILES = MLKListenerController.m MLKToiletApplicationController.m \
                        ToiletLisp_main.m
#ToiletLisp_LOCALIZED_RESOURCE_FILES = MainMenu.nib Credits.rtf
ToiletLisp_RESOURCE_FILES = MainMenu.nib Credits.rtf
#ToiletLisp_MAIN_MODEL_FILE = MainMenu.gorm
ToiletLisp_MAIN_MODEL_FILE = MainMenu.nib
ToiletLisp_OBJCC_FILES = _stamp.mm
ToiletLisp_OBJC_LIBS += -LToiletKit.framework		\
                        -LToiletKit.framework/Versions/Current -lToiletKit
ToiletLisp_OBJCFLAGS = -Wall

ifeq ($(USE_LLVM),YES)
ToiletLisp_OBJC_LIBS += -Lobj -ltoilet-llvm $(LLVM_LDFLAGS)
endif

Test_OBJC_FILES = MLKLowLevelTests.m
Test_OBJC_LIBS = -lUnitKit -LToiletKit.framework -lToiletKit

-include GNUmakefile.preamble
include $(GNUSTEP_MAKEFILES)/application.make
include $(GNUSTEP_MAKEFILES)/bundle.make
include $(GNUSTEP_MAKEFILES)/framework.make
include $(GNUSTEP_MAKEFILES)/library.make
include $(GNUSTEP_MAKEFILES)/tool.make
-include GNUmakefile.postamble

before-all:: before-etshell before-toilet

# _stamp.mm serves two distinct purposes.  First, it causes toilet to be
# relinked whenever one of the $(KIT_TARGETS) has been updated, and
# second, it causes toilet to be linked with g++.
ifeq ($(USE_LLVM),YES)
_stamp.mm: obj/libtoilet-llvm.a
	touch $@
else
_stamp.mm:
	touch $@
endif

before-etshell:: ToiletKit
	rm -f obj/etshell
	mkdir -p $(GNUSTEP_OBJ_DIR)/StepTalkShell

before-Test:: ToiletKit

#after-clean::
#	-rmdir $(GNUSTEP_OBJ_DIR)/StepTalkShell

ifneq ($(BUILD_TOILET_LLVM),YES)
after-clean::
	$(MAKE) clean shared=no BUILD_TOILET_LLVM=YES
endif

test: ToiletKit Test
	env LD_LIBRARY_PATH="`pwd`/ToiletKit.framework/Versions/Current:/usr/local/lib" ukrun Test.bundle

run-et: before-etshell ToiletKit etshell
	env LD_LIBRARY_PATH="`pwd`/ToiletKit.framework/Versions/Current:/usr/local/lib" obj/etshell

run-toilet: $(KIT_TARGETS) toilet
	env LD_LIBRARY_PATH="`pwd`/ToiletKit.framework/Versions/Current:/usr/local/lib" obj/toilet

run-app: $(KIT_TARGETS) ToiletLisp
	env LD_LIBRARY_PATH="`pwd`/ToiletKit.framework/Versions/Current:/usr/local/lib" openapp ./ToiletLisp.app

runapp: run-app

run: run-toilet

debugging-run: $(KIT_TARGETS) toilet
	env LD_LIBRARY_PATH="`pwd`/ToiletKit.framework/Versions/Current:/usr/local/lib" gdb -ex run obj/toilet

ddd-run: $(KIT_TARGETS) toilet
	env LD_LIBRARY_PATH="`pwd`/ToiletKit.framework/Versions/Current:/usr/local/lib" ddd obj/toilet
