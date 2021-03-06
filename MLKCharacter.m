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

#import "MLKCharacter.h"
#import "runtime-compatibility.h"

@implementation MLKCharacter
-(MLKCharacter*) initWithUnichar:(unichar)anUnichar
{
  self = [self init];
  self->unichar = anUnichar;
  return self;
}

+(MLKCharacter*) characterWithUnichar:(unichar)anUnichar
{
  return [[MLKCharacter alloc] initWithUnichar:anUnichar];
}

-(unichar) unicharValue
{
  return self->unichar;
}

-(MLKCharacter *) uppercaseCharacter
{
  return [MLKCharacter characterWithUnichar:[self uppercaseChar]];
}

-(MLKCharacter *) lowercaseCharacter
{
  return [MLKCharacter characterWithUnichar:[self lowercaseChar]];
}

-(unichar) uppercaseChar
{
  return [[[NSString stringWithFormat:@"%C", self->unichar] uppercaseString]
           characterAtIndex:0];
}

-(unichar) lowercaseChar
{
  return [[[NSString stringWithFormat:@"%C", self->unichar] lowercaseString]
           characterAtIndex:0];
}

+(unichar) uppercaseCharForChar:(unichar)ch
{
  return [[MLKCharacter characterWithUnichar:ch] uppercaseChar];
}

+(unichar) lowercaseCharForChar:(unichar)ch
{
  return [[MLKCharacter characterWithUnichar:ch] lowercaseChar];
}

-(BOOL) isEqual:(id)thing
{
  return ([thing isKindOfClass:[MLKCharacter class]]
          && ((MLKCharacter *)thing)->unichar == self->unichar);
}
@end
