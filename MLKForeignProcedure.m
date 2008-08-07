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

#import "MLKForeignProcedure.h"
#import "globals.h"
#import "util.h"

#import <Foundation/NSArray.h>
#import <Foundation/NSEnumerator.h>

#ifdef HAVE_FFI_H
#include <ffi.h>
#elif HAVE_FFI_FFI_H
#include <ffi/ffi.h>
#endif

#include <stdlib.h>
#include <alloca.h>


@implementation MLKForeignProcedure
-(id) initWithCode:(void *)code
     argumentTypes:(NSArray *)argTypes
        returnType:(id)returnType
{
  int i;
  NSEnumerator *e;
  id el;

  self = [super init];

  _code = code;
  _returnType = MLKForeignTypeWithTypeDesignator (returnType);

  _argumentTypes = malloc (sizeof (MLKForeignType) * [argTypes count]);

  e = [argTypes objectEnumerator];
  i = 0;
  while ((el = [e nextObject]))
    {
      _argumentTypes[i++] = MLKForeignTypeWithTypeDesignator (denullify (el));
    }

  return self;
}

-(NSArray *) applyToArray:(NSArray *)arguments
{
  int argc = [arguments count];
  ffi_cif cif;
  ffi_type *arg_types[argc];
  ffi_type *return_type;
  ffi_status status;
  void *argv[argc];
  id return_value;
  int i;

  for (i = 0; i < argc; i++)
    {
      arg_types[i] =
        MLKFFITypeWithForeignType (_argumentTypes[i]);
      argv[i] = alloca (arg_types[i]->size);
      MLKSetForeignValueWithLispValue (argv[i],
                                       [arguments objectAtIndex:i],
                                       _argumentTypes[i]);
    }

  return_type = MLKFFITypeWithForeignType (_returnType);

  status = ffi_prep_cif (&cif, FFI_DEFAULT_ABI, argc, return_type, arg_types);
  if (status != FFI_OK)
    {
      [NSException raise:@"MLKInvalidFFITypeException"
                   format:@"FFI type is invalid (this is probably a bug)."];
    }

  ffi_call (&cif, FFI_FN (_code), &return_value, (void**)argv);

  // FIXME
  if (return_type == &ffi_type_void)
    return [NSArray array];
  else
    return [NSArray arrayWithObject:nullify (MLKLispValueWithForeignValue (&return_value, _returnType))];
}

-(NSString *) description
{
  return MLKPrintToString(self);
}

-(NSString *) descriptionForLisp
{
  return [NSString stringWithFormat:@"<Compiled procedure @%p>", self];
}

-(void) dealloc
{
  // FIXME:  Can we really just use free() here?
  free (_code);
  free (_argumentTypes);
  [super dealloc];
}

-(void) finalize
{
  // FIXME:  Can we really just use free() here?
  free (_code);
  free (_argumentTypes);
}
@end
