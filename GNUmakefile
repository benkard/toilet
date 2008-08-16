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


default: ToiletKit toilet

include $(GNUSTEP_MAKEFILES)/common.make

#all:: ToiletKit etshell Test

TOOL_NAME = etshell toilet
FRAMEWORK_NAME = ToiletKit
BUNDLE_NAME = Test

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

ToiletKit_OBJC_FILES = functions.m globals.m MLKArray.m			\
                       MLKBackquoteReader.m MLKBinding.m MLKCharacter.m	\
                       MLKCommaReader.m MLKCompiledClosure.m MLKCons.m	\
                       MLKDoubleFloat.m					\
                       MLKDispatchingMacroCharacterReader.m		\
                       MLKDynamicContext.m MLKEnvironment.m MLKFloat.m	\
                       MLKForeignProcedure.m MLKForm.m MLKInteger.m	\
                       MLKInterpretedClosure.m MLKInterpreter.m		\
                       MLKLexicalContext.m				\
                       MLKLexicalEnvironment.m MLKNumber.m MLKPackage.m	\
                       MLKParenReader.m MLKQuoteReader.m MLKRatio.m	\
                       MLKReader.m MLKReadtable.m MLKReaderError.m	\
                       MLKRoot.m MLKSemicolonReader.m			\
                       MLKSharpsignColonReader.m MLKSingleFloat.m	\
                       MLKStream.m MLKStringInputStream.m		\
                       MLKStringOutputStream.m MLKStringReader.m	\
                       MLKSymbol.m MLKThrowException.m			\
                       MLKValuesFunction.m NSObject-MLKPrinting.m	\
                       NSString-MLKPrinting.m
ToiletKit_OBJCFLAGS = -Wall
ToiletKit_LDFLAGS = -lgmp -lffi -ldl
#LIBRARIES_DEPEND_UPON

USE_LLVM := YES
ifeq ($(USE_LLVM),YES)
ADDITIONAL_OBJCCFLAGS = $(ADDITIONAL_OBJCFLAGS)
ToiletKit_OBJC_FILES += MLKLexicalContext-MLKLLVMCompilation.m
ToiletKit_OBJCC_FILES = MLKLLVMCompiler.mm
ToiletKit_OBJCFLAGS = -DUSE_LLVM
ToiletKit_OBJCCFLAGS = -DUSE_LLVM `llvm-config --cxxflags` $(ToiletKit_OBJCFLAGS)
ToiletKit_LDFLAGS += `llvm-config --ldflags` `llvm-config --libs backend engine linker codegen transformutils scalaropts analysis ipo`
endif

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
toilet_OBJC_LIBS += -ledit -lncurses -lToiletKit -LToiletKit.framework \
                    -LToiletKit.framework/Versions/Current `llvm-config --ldflags` `llvm-config --libs scalaropts analysis ipo`
toilet_OBJCFLAGS = -Wall

Test_OBJC_FILES = MLKLowLevelTests.m
Test_OBJC_LIBS = -lUnitKit -LToiletKit.framework -lToiletKit

-include GNUmakefile.preamble
include $(GNUSTEP_MAKEFILES)/bundle.make
include $(GNUSTEP_MAKEFILES)/framework.make
include $(GNUSTEP_MAKEFILES)/tool.make
-include GNUmakefile.postamble

before-all:: before-etshell before-toilet

before-toilet:: ToiletKit
	rm -f obj/toilet

before-etshell:: ToiletKit
	rm -f obj/etshell
	mkdir -p $(GNUSTEP_OBJ_DIR)/StepTalkShell

before-Test:: ToiletKit

#after-clean::
#	-rmdir $(GNUSTEP_OBJ_DIR)/StepTalkShell

test: ToiletKit Test
	env LD_LIBRARY_PATH="`pwd`/ToiletKit.framework/Versions/Current:/usr/local/lib" ukrun Test.bundle

run-et: before-etshell ToiletKit etshell
	env LD_LIBRARY_PATH="`pwd`/ToiletKit.framework/Versions/Current:/usr/local/lib" obj/etshell

run-toilet: before-toilet ToiletKit toilet
	env LD_LIBRARY_PATH="`pwd`/ToiletKit.framework/Versions/Current:/usr/local/lib" obj/toilet

run: run-toilet

debugging-run: before-toilet ToiletKit toilet
	env LD_LIBRARY_PATH="`pwd`/ToiletKit.framework/Versions/Current:/usr/local/lib" gdb -ex run obj/toilet

ddd-run: before-toilet ToiletKit toilet
	env LD_LIBRARY_PATH="`pwd`/ToiletKit.framework/Versions/Current:/usr/local/lib" ddd obj/toilet
