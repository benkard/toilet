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

#import "MLKInteger.h"

#import <Foundation/NSString.h>
#include <stdint.h>

#ifdef HAVE_FFI_H
#include <ffi.h>
#elif HAVE_FFI_FFI_H
#include <ffi/ffi.h>
#endif


NSString *MLKPrintToString (id object);

BOOL MLKFixnumP (id thing);
BOOL MLKInstanceP (id thing);

intptr_t MLKIntWithFixnum (id fixnum);
intptr_t MLKIntWithInteger (id integer);
id MLKFixnumWithInt (intptr_t value);
id MLKIntegerWithInt (intptr_t value);
id MLKCanoniseInteger (MLKInteger *x);

id MLKAddFixnums (id x, id y);
id MLKSubtractFixnums (id x, id y);
id MLKIDivideFixnums (id x, id y);
id MLKMultiplyFixnums (id x, id y);

void MLKSplitDeclarationsDocAndForms (id *decls, id *doc, id *forms, id body);

typedef enum MLKForeignType
{
  MLKT_PTR,
  MLKT_SHORT,
  MLKT_USHORT,
  MLKT_INT,
  MLKT_UINT,
  MLKT_LONG,
  MLKT_ULONG,
  MLKT_STRING,
  MLKT_VOID,
  MLKT_BOOL,
  MLKT_ID,
  MLKT_CLASS,
  MLKT_CHAR,
  MLKT_UNICHAR,
  MLKT_ERROR,
  MLKT_INVALID,
} MLKForeignType;

MLKForeignType MLKForeignTypeWithObjectiveCType (const char *typestring);
MLKForeignType MLKForeignTypeWithTypeDesignator (id typeDesignator);
MLKForeignType MLKForeignTypeWithLispValue (id value);
ffi_type *MLKFFITypeWithForeignType (MLKForeignType type);
ffi_type *MLKFFITypeWithObjectiveCType (const char *typestring);
ffi_type *MLKFFITypeWithLispValue (id value);
void MLKSetForeignValueWithLispValue (void *destination, id value, MLKForeignType type);
id MLKLispValueWithForeignValue (void *source, MLKForeignType type);
