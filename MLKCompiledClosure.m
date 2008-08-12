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

#import "MLKCompiledClosure.h"

#import "functions.h"
#import "globals.h"
#import "runtime-compatibility.h"
#import "util.h"

#import <Foundation/NSDictionary.h>
#import <Foundation/NSSet.h>

#import <stdlib.h>


@implementation MLKCompiledClosure
-(id) initWithCode:(void *)code
              data:(id *)data
            length:(intptr_t)dataLength
{
  int i;

  _data = data;
  _dataLength = dataLength;
  _ownPointer = YES;

  _code = malloc (sizeof (id (*)()));
  *_code = code;

  for (i = 0; i < _dataLength; i++)
    {
      LRETAIN (_data[i]);
    }

  return self;
}

+(id) closureWithCode:(void *)code
                 data:(id *)data
               length:(intptr_t)dataLength
{
  return LAUTORELEASE ([[self alloc] initWithCode:code data:data length:dataLength]);
}

-(NSArray *) applyToArray:(NSArray *)arguments
{
  int argc = ([arguments count] + 2);
  ffi_cif cif;
  ffi_type *arg_types[argc];
  ffi_status status;
  void *argv[argc];
  id argpointers[argc - 1];
  id return_value;
  int i;

  arg_types[0] = &ffi_type_pointer;
  argv[0] = &_data;

  for (i = 1; i < argc - 1; i++)
    {
      arg_types[i] = &ffi_type_pointer;
      argpointers[i-1] = denullify([arguments objectAtIndex:i]);
      argv[i] = &argpointers[i-1];
    }

  arg_types[argc - 1] = &ffi_type_pointer;
  argv[argc - 1] = &MLKEndOfArgumentsMarker;

  status = ffi_prep_cif (&cif, FFI_DEFAULT_ABI, argc, &ffi_type_pointer, arg_types);
  if (status != FFI_OK)
    {
      [NSException raise:@"MLKInvalidFFITypeException"
                   format:@"FFI type is invalid (this is probably a bug)."];
    }

  ffi_call (&cif, FFI_FN (*_code), &return_value, (void**)argv);

  // FIXME
  return [NSArray arrayWithObject:nullify(return_value)];
}

-(NSString *) description
{
  return MLKPrintToString (self);
}

-(NSString *) descriptionForLisp
{
  return [NSString stringWithFormat:@"<Compiled closure @%p>", self];
}

-(void) dealloc
{
  int i;

  [super dealloc];

  // FIXME: Decrease refcount of *_code.  Note: When releasing *_code,
  // also release _code regardless of whether we own it.

  for (i = 0; i < _dataLength; i++)
    {
      LRELEASE (_data[i]);
    }
  free (_data);

  if (_ownPointer)
    free (_code);
}
@end
