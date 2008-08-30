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

#import <Foundation/NSString.h>


@interface MLKCharacter : NSObject
{
  unichar unichar;
}

/* Function: -initWithUnichar:

  Initialise character with a given numeric value.
*/
-(MLKCharacter *) initWithUnichar:(unichar)anUnichar;

/* Function: +characterWithUnichar:

  Construct a character with a given numeric value.
*/
+(MLKCharacter *) characterWithUnichar:(unichar)anUnichar;

/* Function: -unicharValue

  Access the character's numeric value.
*/
-(unichar) unicharValue;

/* Function: -uppercaseCharacter

  Return the uppercase form of the character.

  If the character does not have case, -uppercaseCharacter returns self.
*/
-(MLKCharacter *) uppercaseCharacter;

/* Function: -lowercaseCharacter

  Return the lowercase form of the character.

  If the character does not have case, -lowercaseCharacter returns self.
*/
-(MLKCharacter *) lowercaseCharacter;

/* Function: -uppercaseCharacter

  Return the uppercase form of the character as a numeric value.

  If the character does not have case, -uppercaseChar returns [self unicharValue].
*/
-(unichar) uppercaseChar;

/* Function: -uppercaseChar

  Return the lowercase form of the character as a numeric value.

  If the character does not have case, -lowercaseChar returns [self unicharValue].
*/
-(unichar) uppercaseCharForChar;

/* Function: +uppercaseCharForChar:

  Convert a unichar to its corresponding uppercase version.

  If the character does not have case, the argument is returned unchanged.
*/
+(unichar) uppercaseCharForChar:(unichar)ch;

/* Function: +lowercaseCharForChar:

  Convert a unichar to its corresponding lowercase version.

  If the character does not have case, the argument is returned unchanged.
*/
+(unichar) lowercaseCharForChar:(unichar)ch;

/* Function: -isEqual:

  Test whether two <MLKCharacter>s represent the same character.
*/
-(BOOL) isEqual:(id)thing;
@end
