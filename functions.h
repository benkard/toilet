/* -*- mode: objc; coding: utf-8 -*- */
/* Toilet Lisp, a Common Lisp subset for the Étoilé runtime.
 * Copyright (C) 2008, 2009  Matthias Andreas Benkard.
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

/* File: functions.h */

#import "MLKInteger.h"
#import "MLKSymbol.h"

#import <Foundation/NSString.h>
#include <stdint.h>

#ifdef HAVE_FFI_H
#include <ffi.h>
#elif HAVE_FFI_FFI_H
#include <ffi/ffi.h>
#else
#error "Couldn't include ffi.h."
#endif


#ifdef __cplusplus
extern "C" {
#endif

/* Section: Printing */

/* Function: MLKPrintToString

   Build a string describing a Lisp object as if by a simple version of
   PRINT.

   Arguments:

     object - the object to describe.
 */
NSString *MLKPrintToString (id object);


/* Section: Fixnum handling */

/* Function: MLKFixnumP
   
   Test whether an object is a fixnum. */
BOOL MLKFixnumP (id thing);

/* Function: MLKInstanceP
     
   Test whether an object is a real instance (that is, not a fixnum). */
BOOL MLKInstanceP (id thing);

/* Function: MLKIntWithFixnum

   Convert a fixnum into an int. */
intptr_t MLKIntWithFixnum (id fixnum);

/* Function: MLKIntWithInteger

   Convert any Toilet Lisp integer into an int. */
intptr_t MLKIntWithInteger (id integer);

/* Function: MLKFixnumWithInt

   Convert an int into a fixnum. */
id MLKFixnumWithInt (intptr_t value);

/* Function: MLKIntegerWithInt

   Convert an int into a fixnum or MLKInteger depending on its size. */
id MLKIntegerWithInt (intptr_t value);

/* Function: MLKCanoniseInteger

   Test whether a Toilet Lisp integer is an MLKInteger that is too big,
   and if so, convert it to a fixnum. */
id MLKCanoniseInteger (const MLKInteger *x);


/* Section: Fixnum arithmetic */

/* Function: MLKAddFixnums

   Add two fixnums, yielding a Toilet Lisp integer. */
id MLKAddFixnums (id x, id y);

/* Function: MLKSubtractFixnums

   Subtract a fixnum from another, yielding a Toilet Lisp integer. */
id MLKSubtractFixnums (id x, id y);

/* Function: MLKIDivideFixnums

   Divide two fixnums with truncation, yielding a fixnum. */
id MLKIDivideFixnums (id x, id y);

/* Function: MLKMultiplyFixnums

   Multiply two fixnums, yielding a Toilet Lisp integer. */
id MLKMultiplyFixnums (id x, id y);


/* Section: Form parsing */

/* Function: MLKSplitDeclarationsDocAndForms

   Take a form apart into declarations, docstring, and body.

   *decls will be set to a list of declarations.

   *doc will be set to a docstring, if there is one.  Otherwise, it will
   be set to nil except if docp is false, in which case it will simply
   be left alone.

   *forms will be set to the remaining body forms.

   If docp is false, a docstring is not recognised as part of the
   docstring/declaration component of the form.

   Note:

     doc may be NULL if and only if docp is false.

   Arguments:

     decls - a pointer to a cell of type id.
     doc - a pointer to a cell of type id.
     forms - a pointer to a cell of type id.
     body - the form to take apart.
     docp - whether to process docstrings.
*/
void MLKSplitDeclarationsDocAndForms (id *decls, id *doc, id *forms, id body, BOOL docp);

/* Enum: MLKForeignType

   MLKT_PTR - pointer.
   MLKT_SHORT - short int.
   MLKT_USHORT - short unsigned int.
   MLKT_INT - int.
   MLKT_UINT - unsigned int.
   MLKT_LONG - long.
   MLKT_ULONG - unsigned long.
   MLKT_STRING - char *.
   MLKT_VOID - void.
   MLKT_BOOL - C++ bool.
   MLKT_ID - id.
   MLKT_CLASS - class.
   MLKT_CHAR - char.
   MLKT_UNICHAR - unichar.
   MLKT_ERROR - an erroneous type.
   MLKT_INVALID - an unknown or invalid type.
*/
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


/* Section: Foreign type handling */

/* Function: MLKForeignTypeWithObjectiveCType */
MLKForeignType MLKForeignTypeWithObjectiveCType (const char *typestring);

/* Function: MLKForeignTypeWithTypeDesignator */
MLKForeignType MLKForeignTypeWithTypeDesignator (id typeDesignator);

/* Function: MLKForeignTypeWithLispValue */
MLKForeignType MLKForeignTypeWithLispValue (id value);

/* Function: MLKFFITypeWithForeignType */
ffi_type *MLKFFITypeWithForeignType (MLKForeignType type);

/* Function: MLKFFITypeWithObjectiveCType */
ffi_type *MLKFFITypeWithObjectiveCType (const char *typestring);

/* Function: MLKFFITypeWithLispValue */
ffi_type *MLKFFITypeWithLispValue (id value);

/* Function: MLKSetForeignValueWithLispValue */
void MLKSetForeignValueWithLispValue (void *destination, id value, MLKForeignType type);

/* Function: MLKLispValueWithForeignValue */
id MLKLispValueWithForeignValue (void *source, MLKForeignType type);


/* Section: Interpreter-Compiler interoperation. */

/* Function: MLKInterpretedFunctionTrampoline */
id MLKInterpretedFunctionTrampoline (void *target, ...);


#ifdef __cplusplus
}
#endif
