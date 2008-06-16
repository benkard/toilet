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

TOOL_NAME = etoilisp
etoilisp_OBJC_FILES = MLKCharacter.m MLKCons.m MLKDoubleFloat.m		\
                      MLKDynamicContext.m MLKEndOfFileError.m		\
                      MLKEnvironment.m MLKError.m MLKFloat.m		\
                      MLKInteger.m MLKLinkedList.m MLKLispValue.m	\
                      MLKPackage.m MLKParenReader.m MLKRatio.m		\
                      MLKReader.m MLKReadtable.m MLKReaderError.m	\
                      MLKSingleFloat.m MLKStream.m MLKSymbol.m		\
                      MLKThrowException.m				\
                      MLKUndefinedVariableException.m

BUNDLE_NAME = Test
Test_OBJC_FILES = $(etoilisp_OBJC_FILES) MLKLowLevelTests.m
Test_OBJC_LIBS = -lUnitKit

ADDITIONAL_LDFLAGS = -lgmp

-include GNUmakefile.preamble
include $(GNUSTEP_MAKEFILES)/bundle.make
include $(GNUSTEP_MAKEFILES)/tool.make
-include GNUmakefile.postamble

test: Test
	ukrun Test.bundle
