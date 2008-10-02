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

#import "MLKSymbol.h"
#import "MLKPackage.h"

static MLKPackage *cl = nil;
static MLKPackage *sys;
static MLKPackage *keyword;

static MLKSymbol *IF;
static MLKSymbol *IN_PACKAGE;
static MLKSymbol *DECLARE;
static MLKSymbol *PROGN;
static MLKSymbol *TAGBODY;
static MLKSymbol *GO;
static MLKSymbol *CATCH;
static MLKSymbol *THROW;
static MLKSymbol *_FLET;
static MLKSymbol *_MACROLET;
static MLKSymbol *LAMBDA;
static MLKSymbol *LET;
static MLKSymbol *LOCALLY;
static MLKSymbol *FUNCALL;
static MLKSymbol *FUNCTION;
static MLKSymbol *EVAL_WHEN;
static MLKSymbol *QUOTE;
static MLKSymbol *SETQ;
static MLKSymbol *SETF;
static MLKSymbol *_FSETQ;
static MLKSymbol *SYMBOL_MACROLET;
static MLKSymbol *PROGV;
static MLKSymbol *UNWIND_PROTECT;
static MLKSymbol *VALUES;
static MLKSymbol *_FOREIGN_LAMBDA;
static MLKSymbol *_LAMBDA;
static MLKSymbol *_LOOP;
static MLKSymbol *V_INITP;
static MLKSymbol *COMPILE_TOPLEVEL;
static MLKSymbol *COMPILE;
static MLKSymbol *LOAD_TOPLEVEL;
static MLKSymbol *LOAD;
static MLKSymbol *EXECUTE;
static MLKSymbol *MULTIPLE_VALUE_LIST;
static MLKSymbol *INLINE;
static MLKSymbol *NOTINLINE;
static MLKSymbol *SPECIAL;
static MLKSymbol *LEXICAL;
static MLKSymbol *QUERY_IO;
static MLKSymbol *ERROR_OUTPUT;
static MLKSymbol *STANDARD_OUTPUT;
static MLKSymbol *TERMINAL_IO;
static MLKSymbol *TRACE_OUTPUT;
static MLKSymbol *DEBUG_IO;


static void
ensure_symbols ()
{
  if (cl) return;

  cl = [MLKPackage findPackage:@"COMMON-LISP"];
  sys = [MLKPackage findPackage:@"TOILET-SYSTEM"];
  keyword = [MLKPackage findPackage:@"KEYWORD"];

  IF = [cl intern:@"IF"];
  IN_PACKAGE = [cl intern:@"IN-PACKAGE"];
  DECLARE = [cl intern:@"DECLARE"];
  PROGN = [cl intern:@"PROGN"];
  TAGBODY = [cl intern:@"TAGBODY"];
  GO = [cl intern:@"GO"];
  CATCH = [cl intern:@"CATCH"];
  THROW = [cl intern:@"THROW"];
  LAMBDA = [cl intern:@"LAMBDA"];
  LET = [cl intern:@"LET"];
  LOCALLY = [cl intern:@"LOCALLY"];
  _FLET = [sys intern:@"%FLET"];
  _MACROLET = [sys intern:@"%MACROLET"];
  _LOOP = [sys intern:@"%LOOP"];
  EVAL_WHEN = [cl intern:@"EVAL-WHEN"];
  FUNCALL = [cl intern:@"FUNCALL"];
  FUNCTION = [cl intern:@"FUNCTION"];
  QUOTE = [cl intern:@"QUOTE"];
  SETQ = [cl intern:@"SETQ"];
  SETF = [cl intern:@"SETF"];
  _FSETQ = [sys intern:@"%FSETQ"];
  SYMBOL_MACROLET = [cl intern:@"SYMBOL-MACROLET"];
  PROGV = [cl intern:@"PROGV"];
  VALUES = [cl intern:@"VALUES"];
  UNWIND_PROTECT = [cl intern:@"UNWIND-PROTECT"];
  _FOREIGN_LAMBDA = [sys intern:@"%FOREIGN-LAMBDA"];
  _LAMBDA = [sys intern:@"%LAMBDA"];
  V_INITP = [sys intern:@"*SYSTEM-INITIALISED-P*"];
  MULTIPLE_VALUE_LIST = [cl intern:@"MULTIPLE-VALUE-LIST"];
  INLINE = [cl intern:@"INLINE"];
  NOTINLINE = [cl intern:@"NOTINLINE"];
  SPECIAL = [cl intern:@"INLINE"];
  LEXICAL = [sys intern:@"NOTINLINE"];

  QUERY_IO = [cl intern:@"*QUERY-IO*"];
  ERROR_OUTPUT = [cl intern:@"*ERROR-OUTPUT*"];
  STANDARD_OUTPUT = [cl intern:@"*STANDARD-OUTPUT*"];
  TERMINAL_IO = [cl intern:@"*TERMINAL-IO*"];
  TRACE_OUTPUT = [cl intern:@"*TRACE-OUTPUT*"];
  DEBUG_IO = [cl intern:@"*DEBUG-IO*"];

  COMPILE_TOPLEVEL = [keyword intern:@"COMPILE-TOPLEVEL"];
  COMPILE = [cl intern:@"COMPILE"];
  LOAD_TOPLEVEL = [keyword intern:@"LOAD-TOPLEVEL"];
  LOAD = [cl intern:@"LOAD"];
  EXECUTE = [keyword intern:@"EXECUTE"];
}
