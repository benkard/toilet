## Étoilisp, a Common Lisp subset for Étoilé.
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


include $(GNUSTEP_MAKEFILES)/common.make

#all:: ToiletKit etshell Test

ADDITIONAL_OBJCFLAGS = -Wall

FRAMEWORK_NAME = ToiletKit
ToiletKit_OBJC_FILES = MLKCharacter.m MLKCons.m MLKDoubleFloat.m	\
                       MLKDynamicContext.m MLKEndOfFileError.m		\
                       MLKEnvironment.m MLKError.m MLKFloat.m		\
                       MLKInteger.m MLKLinkedList.m MLKLispValue.m	\
                       MLKPackage.m MLKParenReader.m MLKRatio.m		\
                       MLKReader.m MLKReadtable.m MLKReaderError.m	\
                       MLKSingleFloat.m MLKStream.m			\
                       MLKStringInputStream.m MLKSymbol.m		\
                       MLKThrowException.m				\
                       MLKUndefinedVariableException.m			\
                       NSObject-MLKPrinting.m NSString-MLKPrinting.m
ToiletKit_LDFLAGS = -lgmp
#LIBRARIES_DEPEND_UPON

#TOOL_NAME = etoilet
#etoilet_OBJC_FILES = main.m
#etoilet_OBJC_LIBS = -lToiletKit -LToiletKit.framework

TOOL_NAME = etshell
etshell_OBJC_FILES = StepTalkShell/STShell.m		\
                     StepTalkShell/STShell+output.m	\
                     StepTalkShell/stshell_tool.m
etshell_OBJC_LIBS += -lStepTalk -lreadline -lncurses -lToiletKit	\
                     -LToiletKit.framework
etshell_OBJCFLAGS = -w

BUNDLE_NAME = Test
Test_OBJC_FILES = $(ToiletKit_OBJC_FILES) MLKLowLevelTests.m
Test_OBJC_LIBS = -lUnitKit

-include GNUmakefile.preamble
include $(GNUSTEP_MAKEFILES)/bundle.make
include $(GNUSTEP_MAKEFILES)/framework.make
include $(GNUSTEP_MAKEFILES)/tool.make
-include GNUmakefile.postamble

before-all:: before-etshell

before-etshell::
	rm -f obj/etshell
	mkdir -p $(GNUSTEP_OBJ_DIR)/StepTalkShell

#after-clean::
#	-rmdir $(GNUSTEP_OBJ_DIR)/StepTalkShell

test: Test
	ukrun Test.bundle

run: before-etshell ToiletKit etshell
	env LD_LIBRARY_PATH=`pwd`/ToiletKit.framework/Versions/Current:/usr/local/lib obj/etshell
