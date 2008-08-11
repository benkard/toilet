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

#import "functions.h"
#import "globals.h"
#import "util.h"
#import "MLKCons.h"
#import "MLKCharacter.h"
#import "MLKInteger.h"
#import "MLKInterpretedClosure.h"
#import "MLKPackage.h"
#import "MLKSymbol.h"

#import <Foundation/NSException.h>
#import <Foundation/NSString.h>

#include <string.h>
#include <stdarg.h>
#include <alloca.h>


NSString *MLKPrintToString (id object)
{
  if (object == nil)
    return @"()";
  else if (MLKInstanceP (object))
    return [object descriptionForLisp];
  else if (MLKFixnumP (object))
    return MLKPrintToString ([MLKInteger
                               integerWithIntptr_t:(MLKIntWithFixnum (object))]);
  else
    {
      NSLog (@"MLKPrintToString: Encountered a really weird object at address %p",
             object);
      return @"<??\?>";
    }
}

intptr_t MLKIntWithFixnum (id fixnum)
{
  return ((intptr_t)fixnum >> 1);
}

id MLKFixnumWithInt (intptr_t value)
{
  return (id)((value << 1) | 1);
}

intptr_t MLKIntWithInteger (id integer)
{
  if (MLKFixnumP (integer))
    return MLKIntWithFixnum (integer);
  else
    return [integer intValue];
}

id MLKIntegerWithInt (intptr_t value)
{
#ifndef NO_FIXNUMS
  intptr_t maybeFixnum = (value << 1) | 1;
  if (value == (maybeFixnum >> 1))
    return (id)maybeFixnum;
  else
#endif
    return [MLKInteger integerWithIntptr_t:value];
}

BOOL MLKFixnumP (id thing)
{
  return ((intptr_t)thing & 1);
}

BOOL MLKInstanceP (id thing)
{
  return !((intptr_t)thing & 1);
}

id MLKCanoniseInteger (MLKInteger *x)
{
  if (MLKFixnumP (x))
    {
      return x;
    }
  else if (MLKInstanceP (x))
    {
      if ([x fitsIntoFixnum])
        return [x fixnumValue];
      else
        return x;
    }
  else
    {
      NSLog (@"MLKCanoniseInteger: Encountered a really weird object at address %p",
             x);
      return 0;
    }
}

id MLKAddFixnums (id x, id y)
{
  intptr_t ix = MLKIntWithFixnum (x);
  intptr_t iy = MLKIntWithFixnum (y);
  intptr_t result = ix + iy;

  return MLKIntegerWithInt (result);
}

id MLKSubtractFixnums (id x, id y)
{
  intptr_t ix = MLKIntWithFixnum (x);
  intptr_t iy = MLKIntWithFixnum (y);
  intptr_t result = ix - iy;

  return MLKIntegerWithInt (result);
}

id MLKIDivideFixnums (id x, id y)
{
  intptr_t ix = MLKIntWithFixnum (x);
  intptr_t iy = MLKIntWithFixnum (y);
  intptr_t result = ix / iy;

  return MLKIntegerWithInt (result);
}

id MLKMultiplyFixnums (id x, id y)
{
  id ix = [MLKInteger integerWithFixnum:x];
  id iy = [MLKInteger integerWithFixnum:y];
  id result = [ix multiplyWith:iy];

  return MLKCanoniseInteger (result);
}


static MLKSymbol *INT, *SHORT, *LONG, *VOID, *POINTER,
  *UINT, *USHORT, *ULONG, *STRING, *ID, *BOOLEAN, *CLASS, *UNICHAR, *CHAR,
  *ERROR;
static MLKSymbol *DECLARE;
static MLKPackage *keyword = nil, *cl = nil;

#define INTERN_KEYWORD(VAR, NAME)               \
  LASSIGN (VAR, [keyword intern:NAME])

static void init_symbols ()
{
  if (keyword)
    return;

  cl = [MLKPackage findPackage:@"COMMON-LISP"];
  keyword = [MLKPackage findPackage:@"KEYWORD"];
  INTERN_KEYWORD (POINTER, @"POINTER");
  INTERN_KEYWORD (SHORT, @"SHORT-INT");
  INTERN_KEYWORD (USHORT, @"UNSIGNED-SHORT-INT");
  INTERN_KEYWORD (INT, @"INT");
  INTERN_KEYWORD (UINT, @"UNSIGNED-INT");
  INTERN_KEYWORD (LONG, @"LONG-INT");
  INTERN_KEYWORD (ULONG, @"UNSIGNED-LONG-INT");
  INTERN_KEYWORD (STRING, @"STRING");
  INTERN_KEYWORD (ID, @"ID");
  INTERN_KEYWORD (BOOLEAN, @"BOOL");
  INTERN_KEYWORD (CLASS, @"CLASS");
  INTERN_KEYWORD (UNICHAR, @"UNICHAR");
  INTERN_KEYWORD (CHAR, @"CHAR");
  INTERN_KEYWORD (ERROR, @"ERROR");
  INTERN_KEYWORD (VOID, @"VOID");

  DECLARE = [cl intern:@"DECLARE"];
}


void MLKSplitDeclarationsDocAndForms (id *decls, id *doc, id *forms, id body, BOOL docp)
{
  id declarations;

  init_symbols ();

  if (docp)
    *doc = nil;

  declarations = nil;
  while (([[body car] isKindOfClass:[MLKCons class]]
          && [[body car] car] == DECLARE)
         || (docp && [[body car] isKindOfClass:[NSString class]]))
    {
      id thing = [body car];

      if ([thing isKindOfClass:[NSString class]])
        {
          if (*doc)
            {
              body = [body cdr];
              break;
            }
          else
            {
              *doc = thing;
            }
        }
      else
        {
          thing = [thing cdr];

          if (declarations)
            declarations = [declarations listByAppendingObject:thing];
          else
            declarations = thing;
        }

      body = [body cdr];
    }

  *decls = declarations;
  *forms = body;
}


MLKForeignType MLKForeignTypeWithTypeDesignator (id typeDesignator)
{
#define DESIGNATOR_CASE(TYPE, FOREIGN_TYPE)                     \
  if (typeDesignator == TYPE) return MLKT_ ## FOREIGN_TYPE;

  init_symbols ();

  DESIGNATOR_CASE (POINTER, PTR)
  else DESIGNATOR_CASE (INT, INT)
  else DESIGNATOR_CASE (UINT, UINT)
  else DESIGNATOR_CASE (SHORT, SHORT)
  else DESIGNATOR_CASE (LONG, LONG)
  else DESIGNATOR_CASE (USHORT, USHORT)
  else DESIGNATOR_CASE (ULONG, ULONG)
  else DESIGNATOR_CASE (STRING, STRING)
  else DESIGNATOR_CASE (VOID, VOID)
  else DESIGNATOR_CASE (ID, ID)
  else DESIGNATOR_CASE (BOOLEAN, BOOL)
  else DESIGNATOR_CASE (CLASS, CLASS)
  else DESIGNATOR_CASE (UNICHAR, UNICHAR)
  else DESIGNATOR_CASE (CHAR, CHAR)
  else DESIGNATOR_CASE (ERROR, ERROR)
  else return MLKT_INVALID;
}

ffi_type *MLKFFITypeWithForeignType (MLKForeignType type)
{
#define FFI_TYPE_CASE(FOREIGN_TYPE, FFI_TYPE)                   \
  case MLKT_ ## FOREIGN_TYPE: return &(ffi_type_ ## FFI_TYPE);

  switch (type)
    {
    FFI_TYPE_CASE (PTR, pointer);
    FFI_TYPE_CASE (INT, sint);
    FFI_TYPE_CASE (UINT, uint);
    FFI_TYPE_CASE (LONG, slong);
    FFI_TYPE_CASE (ULONG, ulong);
    FFI_TYPE_CASE (SHORT, sshort);
    FFI_TYPE_CASE (USHORT, ushort);
    FFI_TYPE_CASE (STRING, pointer);
    FFI_TYPE_CASE (VOID, void);
    FFI_TYPE_CASE (ID, pointer);
    FFI_TYPE_CASE (BOOL, schar);
    FFI_TYPE_CASE (CLASS, pointer);
    FFI_TYPE_CASE (UNICHAR, sshort);
    FFI_TYPE_CASE (CHAR, schar);
    FFI_TYPE_CASE (ERROR, pointer);
    case MLKT_INVALID: return NULL;
    }

  return NULL;
}

static id truify (BOOL value)
{
  init_symbols ();
  return (value ? (id) [cl intern:@"T"] : nil);
}

id MLKLispValueWithForeignValue (void *source, MLKForeignType type)
{
  switch (type)
    {
    case MLKT_INT: return MLKIntegerWithInt(*(int*)source);
    case MLKT_UINT: return MLKIntegerWithInt(*(unsigned*)source);
    case MLKT_LONG: return MLKIntegerWithInt(*(long*)source);
    case MLKT_ULONG: return MLKIntegerWithInt(*(unsigned long*)source);
    case MLKT_SHORT: return MLKIntegerWithInt(*(short*)source);
    case MLKT_USHORT: return MLKIntegerWithInt(*(unsigned short*)source);
    case MLKT_PTR: return MLKIntegerWithInt(*(intptr_t*)source);  //FIXME
    case MLKT_STRING: return [NSString stringWithUTF8String:(*(char**)source)];
    case MLKT_ID: return *(id*)source;
    case MLKT_BOOL: return truify (*(BOOL*)source);
    case MLKT_CLASS: return *(Class*)source;
    case MLKT_UNICHAR: return [MLKCharacter characterWithUnichar:(*(unichar*)source)];
    case MLKT_CHAR: return [MLKCharacter characterWithUnichar:(*(char*)source)];
    case MLKT_ERROR: return *(id*)source;
//    case MLKT_: return (*(*)source);
    case MLKT_INVALID: return nil;
    case MLKT_VOID: return nil;
    }

  return nil;
}

void MLKSetForeignValueWithLispValue (void *destination, id value, MLKForeignType type)
{
  switch (type)
    {
    case MLKT_INT: *(int *)destination = MLKIntWithInteger (value); break;
    case MLKT_UINT: *(unsigned *)destination = MLKIntWithInteger (value); break;
    case MLKT_LONG: *(long *)destination = MLKIntWithInteger (value); break;
    case MLKT_ULONG: *(unsigned long *)destination = MLKIntWithInteger (value); break;
    case MLKT_SHORT: *(short *)destination = MLKIntWithInteger (value); break;
    case MLKT_USHORT: *(unsigned short *)destination = MLKIntWithInteger (value); break;
    case MLKT_PTR: *(void **)destination = value; break;
    case MLKT_STRING: *(const char **)destination = [value UTF8String]; break;
    case MLKT_ID:
      *(id*)destination = (MLKFixnumP (value)
                           ? (id)[MLKInteger integerWithFixnum:value]
                           : (id)value);
      break;
    case MLKT_BOOL: *(BOOL*)destination = (value != nil); break;
    case MLKT_CLASS: *(Class*)destination = (Class)value; break;
    case MLKT_UNICHAR:
      if (MLKFixnumP (value))
        *(unichar*)destination = MLKIntWithFixnum (value);
      else if ([value isKindOfClass:[MLKInteger class]])
        *(unichar*)destination = [value intValue];
      else        
        *(unichar*)destination = [value unicharValue];
      break;
    case MLKT_CHAR: *(char*)destination = [value unicharValue]; break;
    case MLKT_ERROR: *(id*)destination = value; break;
//    case MLKT_: *(*)destination = ; break;
    case MLKT_INVALID: break;
    case MLKT_VOID: break;
    }
}

MLKForeignType MLKForeignTypeWithObjectiveCType (const char *typestring)
{
#define OBJC_TYPE_CASE(OBJCTYPE, TYPE)                          \
  if (strcmp (typestring, @encode(OBJCTYPE)) == 0) return MLKT_ ## TYPE;

  OBJC_TYPE_CASE (BOOL, BOOL)
  else OBJC_TYPE_CASE (unichar, UNICHAR)
  else OBJC_TYPE_CASE (char, CHAR)
  else OBJC_TYPE_CASE (int, INT)
  else OBJC_TYPE_CASE (short, SHORT)
  else OBJC_TYPE_CASE (long, LONG)
  else OBJC_TYPE_CASE (unsigned, UINT)
  else OBJC_TYPE_CASE (unsigned short, USHORT)
  else OBJC_TYPE_CASE (unsigned long, ULONG)
  else OBJC_TYPE_CASE (void *, PTR)
  else OBJC_TYPE_CASE (char *, STRING)
  else OBJC_TYPE_CASE (id, ID)
  else OBJC_TYPE_CASE (Class, CLASS)
  else OBJC_TYPE_CASE (NSException *, ERROR)
  else OBJC_TYPE_CASE (void, VOID)
  else return MLKT_INVALID;
}


MLKForeignType MLKForeignTypeWithLispValue (id value);
ffi_type *MLKFFITypeWithObjectiveCType (const char *typestring);
ffi_type *MLKFFITypeWithLispValue (id value);


id MLKInterpretedFunctionTrampoline (void *target, ...)
{
  // Our first argument is the fat pointer's closure data pointer.  We
  // simply treat it as a pointer to the MLKInterpretedClosure that we
  // want to call, because that is what we put there when setting this
  // trampoline up with a specific MLKInterpretedClosure.

  // FIXME: Implement multiple-value return, or at least set the
  // multiple-value return flag to 0 before doing anything else.

  NSArray *values;
  NSMutableArray *arguments = [NSMutableArray array];
  MLKInterpretedClosure *closure = target;
  id arg;
  va_list ap;

  va_start (ap, target);
  while ((arg = va_arg (ap, id)) != MLKEndOfArgumentsMarker)
    {
      [arguments addObject:nullify(arg)];
    }
  va_end (ap);

  values = [closure applyToArray:arguments];

  if ([values count] > 0)
    return [values objectAtIndex:0];
  else
    return nil;
}
