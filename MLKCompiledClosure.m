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
#import "MLKCons.h"

#import "functions.h"
#import "globals.h"
#import "runtime-compatibility.h"
#import "util.h"

#import <Foundation/NSDictionary.h>
#import <Foundation/NSSet.h>

#ifdef __OBJC_GC__
#import <Foundation/NSZone.h>
#endif

#import <stdlib.h>


@implementation MLKCompiledClosure
-(id) initWithCode:(void *)code
              data:(id *)data
            length:(intptr_t)dataLength
{
  int i;

  m_dataLength = dataLength;
  m_code = code;

#ifdef __OBJC_GC__
  m_data = NSAllocateCollectable (dataLength * sizeof(id), NSScannedOption);
#else
  m_data = malloc (dataLength * sizeof(id));
#endif

  for (i = 0; i < m_dataLength; i++)
    {
      m_data[i] = LRETAIN (data[i]);
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
  int argc = ([arguments count] + 3);
  ffi_cif cif;
  ffi_type *arg_types[argc];
  ffi_status status;
  void *argv[argc];
  id argpointers[argc - 2];
  ffi_arg return_value;
  int i;
  id return_values = nil;
  id *return_values_ptr = &return_values;

  arg_types[0] = &ffi_type_pointer;
  argv[0] = &m_data;
  
  arg_types[1] = &ffi_type_pointer;
  argv[1] = &return_values_ptr;

  for (i = 2; i < argc - 1; i++)
    {
      arg_types[i] = &ffi_type_pointer;
      argpointers[i-2] = denullify([arguments objectAtIndex:(i-2)]);
      argv[i] = &argpointers[i-2];
    }

  arg_types[argc - 1] = &ffi_type_pointer;
  argv[argc - 1] = &MLKEndOfArgumentsMarker;

  status = ffi_prep_cif (&cif, FFI_DEFAULT_ABI, argc, &ffi_type_pointer, arg_types);
  if (status != FFI_OK)
    {
      [NSException raise:@"MLKInvalidFFITypeException"
                   format:@"FFI type is invalid (this is probably a bug)."];
    }

//   NSLog (@"Calling %p (argc = %d)", _code, argc);
//   for (i = 0; i < argc; i++)
//     {
//       NSLog (@"Argument %d: %p", i, *((void**)argv[i]));
//     }

  ffi_call (&cif, FFI_FN (m_code), &return_value, (void**)argv);
//  return_value = ((id (*)(void *, ...))_code) (_data, argpointers[0], argpointers[1], MLKEndOfArgumentsMarker);

  if (return_values)
    {
      MLKCons *values = [return_values cdr];
      return (values ? (id)[values array] : (id)[NSArray array]);
    }
  else
    {
      return [NSArray arrayWithObject:nullify((id)return_value)];
    }
}

-(NSString *) description
{
  return MLKPrintToString (self);
}

-(NSString *) descriptionForLisp
{
  return [NSString stringWithFormat:@"<Compiled closure @%p>", self];
}

-(id (*)()) code
{
  return m_code;
}

-(void *) closureData
{
  return m_data;
}

-(void) dealloc
{
  int i;

  [super dealloc];

  // FIXME: Decrease refcount of _code.
  for (i = 0; i < m_dataLength; i++)
    {
      LRELEASE (m_data[i]);
    }
  free (m_data);
}
@end
