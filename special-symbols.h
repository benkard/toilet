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
static MLKSymbol *APPLY;
static MLKSymbol *FUNCALL;
static MLKSymbol *FUNCTION;
static MLKSymbol *EVAL;
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
static MLKSymbol *MULTIPLE_VALUE_CALL;


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
  APPLY = [cl intern:@"APPLY"];
  EVAL = [cl intern:@"EVAL"];
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
  MULTIPLE_VALUE_CALL = [cl intern:@"MULTIPLE-VALUE-CALL"];

  COMPILE_TOPLEVEL = [keyword intern:@"COMPILE-TOPLEVEL"];
  COMPILE = [cl intern:@"COMPILE"];
  LOAD_TOPLEVEL = [keyword intern:@"LOAD-TOPLEVEL"];
  LOAD = [cl intern:@"LOAD"];
  EXECUTE = [keyword intern:@"EXECUTE"];
}
