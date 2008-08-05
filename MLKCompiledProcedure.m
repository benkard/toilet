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

#import "MLKCompiledProcedure.h"
#import "globals.h"
#import "util.h"

#import <Foundation/NSArray.h>

#ifdef HAVE_FFI_H
#include <ffi.h>
#elif HAVE_FFI_FFI_H
#include <ffi/ffi.h>
#endif

#include <stdlib.h>


@implementation MLKCompiledProcedure
-(id) initWithCode:(void *)code
{
  self = [super init];
  _code = code;
  return self;
}

-(NSArray *) applyToArray:(NSArray *)arguments
{
  int argc = ([arguments count] + 1);
  ffi_cif cif;
  ffi_type *arg_types[argc];
  ffi_status status;
  id *argv[argc];
  id argpointers[argc - 1];
  id return_value;
  int i;

  for (i = 0; i < argc - 1; i++)
    {
      arg_types[i] = &ffi_type_pointer;
      argpointers[i] = denullify([arguments objectAtIndex:i]);
      argv[i] = &argpointers[i];
    }

  arg_types[argc - 1] = &ffi_type_pointer;
  argv[argc - 1] = &MLKEndOfArgumentsMarker;

  status = ffi_prep_cif (&cif, FFI_DEFAULT_ABI, argc, &ffi_type_pointer, arg_types);
  if (status != FFI_OK)
    {
      [NSException raise:@"MLKInvalidFFITypeException"
                   format:@"FFI type is invalid (this is probably a bug)."];
    }

  ffi_call (&cif, FFI_FN (_code), &return_value, (void**)argv);

  // FIXME
  return [NSArray arrayWithObject:nullify(return_value)];
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
  [super dealloc];
}
@end
