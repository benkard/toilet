/* -*- mode: objc; coding: utf-8 -*- */
/* Étoilisp/Mulklisp, a Common Lisp subset for the Étoilé runtime.
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
#import "MLKInteger.h"

NSString *MLKPrintToString (id object)
{
  if (object == nil)
    return @"()";
  else if (MLKInstanceP (object))
    return [object descriptionForLisp];
  else if (MLKFixnumP (object))
    return MLKPrintToString ([MLKInteger
                               integerWithInt:(MLKIntWithFixnum (object))]);
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

id MLKIntegerWithInt (intptr_t value)
{
  intptr_t maybeFixnum = ((intptr_t)value << 1) | 1;
  if (value == (maybeFixnum >> 1))
    return (id)maybeFixnum;
  else
    return [MLKInteger integerWithInt:value];
}
 
BOOL MLKFixnumP (id thing)
{
  return ((intptr_t)thing & 1);
}

BOOL MLKInstanceP (id thing)
{
  return !((intptr_t)thing & 1);
}
