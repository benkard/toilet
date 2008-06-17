#
#  StepTalk tools
#
#  Copyright (C) 2002 Stefan Urbanek
#
#  This file is part of the StepTalk.
#
#  This library is free software; you can redistribute it and/or
#  modify it under the terms of the GNU Library General Public
#  License as published by the Free Software Foundation; either
#  version 2 of the License, or (at your option) any later version.
#
#  This library is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
#  Library General Public License for more details.
#
#  You should have received a copy of the GNU Library General Public
#  License along with this library; if not, write to the Free
#  Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02111, USA.
#

include $(GNUSTEP_MAKEFILES)/common.make

TOOL_NAME = stshell

stshell_OBJC_FILES = STShell.m STShell+output.m stshell_tool.m

ADDITIONAL_TOOL_LIBS += -lStepTalk -lreadline -lncurses -lToiletKit -L../ToiletKit.framework

ADDITIONAL_OBJCFLAGS = -Wall -Wno-import

-include GNUmakefile.preamble
include $(GNUSTEP_MAKEFILES)/tool.make
-include GNUMakefile.postamble
